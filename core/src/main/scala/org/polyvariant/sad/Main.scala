/*
 * Copyright 2022 Polyvariant
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.polyvariant.sad

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.ShapeType.Category
import software.amazon.smithy.model.shapes.ShapeVisitor
import software.amazon.smithy.model.shapes.StructureShape

import java.nio.file.Files
import java.nio.file.Paths
import java.util.stream
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object Main extends App {

  def makeModel(
    loader: ClassLoader
  ): Model = Model
    .assembler(loader)
    // .discoverModels(loader)
    .addUnparsedModel(
      "demo.smithy",
      Files.readString(
        Paths.get(
          "core/src/main/scala/org/polyvariant/sad/after.smithy"
        )
      ),
    )
    .assemble()
    .unwrap()

  def makeModel2() = Model
    .assembler()
    .addUnparsedModel(
      "demo.smithy",
      Files.readString(
        Paths.get(
          "core/src/main/scala/org/polyvariant/sad/before.smithy"
        )
      ),
    )
    .assemble()
    .unwrap()

  val newModel = makeModel(getClass.getClassLoader())
  val oldModel = makeModel2()

  implicit class StreamOps[A](s: stream.Stream[A]) {
    def toScalaList: List[A] = s.collect(Collectors.toList()).asScala.toList
  }

  import DiffTree._

  implicit class StringColorOps(s: String) {
    def yellow: String = Console.YELLOW + s + Console.RESET
    def red: String = Console.RED + s + Console.RESET
    def green: String = Console.GREEN + s + Console.RESET
  }

  def renderStructChange(sc: StructChange, depth: Int): String = {
    val child = renderDiffTree(sc.diff, depth + 1)

    ("\n" + (" " * depth * 2)) + "~ " + s"${sc.name.yellow}: $child"
  }

  def renderDiffTree(dt: DiffTree, depth: Int): String =
    dt match {
      case sc: StructChange => renderStructChange(sc, depth)
      case tc: TypeChange   => s"${tc.before.red} -> ${tc.after.green}"
      case Todo             => "todo"
    }

  val visitor = new ToDiffTreeVisitor(oldModel, newModel)
  oldModel.getServiceShapes().asScala.toList.foreach { service =>
    val dt = service
      .accept(visitor)
      // todo: support removal of services
      .run(Context(newModel.expectShape(service.getId())))

    println(renderDiffTree(dt, depth = 0))
  }
}

sealed trait DiffTree extends Product with Serializable {
  def inStruct(name: String): DiffTree.StructChange = DiffTree.StructChange(name, this)
}

object DiffTree {
  case class StructChange(name: String, diff: DiffTree) extends DiffTree
  case class TypeChange(before: String, after: String) extends DiffTree
  case object Todo extends DiffTree

}

// operates on shapes of the old model, as they're the base for our diffs

case class Context(correspondingShape: Shape)

// never too many things called compilers
trait DiffCompiler {
  def run(ctx: Context): DiffTree

  // DiffCompiler that moves the context down using the `f` function.
  def down[S <: Shape](f: S => Shape): DiffCompiler =
    ctx =>
      run(
        ctx.copy(correspondingShape = f(ctx.correspondingShape.asInstanceOf[S]))
      )

  def map(f: DiffTree => DiffTree): DiffCompiler = ctx => f(run(ctx))

}

class ToDiffTreeVisitor(oldModel: Model, newModel: Model)
  extends ShapeVisitor.Default[DiffCompiler] {

  import DiffTree._

  implicit class ShapeIDOps(id: ShapeId) {
    def resolved(model: Model): Shape = model.expectShape(id)
  }

  override def getDefault(
    shape: software.amazon.smithy.model.shapes.Shape
  ): DiffCompiler =
    ctx =>
      if (
        shape.getType().getCategory() == Category.SIMPLE &&
        ctx.correspondingShape.getType().getCategory() == Category.SIMPLE
      )
        TypeChange(
          shape.getType().toString(),
          ctx.correspondingShape.getType().toString(),
        )
      else
        ???

  override def operationShape(shape: OperationShape): DiffCompiler = {

    def f(model: Model)(shape: OperationShape) = shape
      .getInput()
      .get() /* todo */
      .resolved(model)
      .asStructureShape()
      .get()

    structureShape(
      f(oldModel)(shape)
    ).down(f(newModel)).map(_.inStruct("input").inStruct(shape.getId().toString()))
  }

  override def structureShape(shape: StructureShape): DiffCompiler = {
    def f(shape: StructureShape) =
      shape
        .members()
        .asScala
        .head

    f(shape)
      .accept(this)
      .down(f)
  }

  override def memberShape(shape: MemberShape): DiffCompiler = {
    def f(model: Model)(shape: MemberShape): Shape = shape.getTarget().resolved(model)

    f(oldModel)(shape)
      .accept(this)
      .down(f(newModel))
      .map(_.inStruct(shape.getMemberName()))
  }

  override def serviceShape(shape: ServiceShape): DiffCompiler = {

    def f(model: Model)(shape: ServiceShape) = shape
      .getAllOperations()
      .asScala
      .head /* todo */
      .resolved(model)

    f(oldModel)(shape)
      .accept(this)
      .down(f(newModel))
      .map { child =>
        child.inStruct("operations").inStruct(shape.getId().toString())
      }
  }

}
