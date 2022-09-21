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

  sealed trait DiffTree extends Product with Serializable
  case class StructChange(name: String, diff: DiffTree) extends DiffTree
  case class TypeChange(before: String, after: String) extends DiffTree
  case object Todo extends DiffTree

  implicit class StringColorOps(s: String) {
    def yellow: String = Console.YELLOW + s + Console.RESET
    def red: String = Console.RED + s + Console.RESET
    def green: String = Console.GREEN + s + Console.RESET
  }

  implicit class ShapeIDOps(id: ShapeId) {
    def resolved(model: Model): Shape = model.expectShape(id)
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

  // operates on shapes of the old model, as they're the base for our diffs
  val toDiffTreeVisitor =
    new ShapeVisitor.Default[DiffTree] {

      override def getDefault(
        shape: software.amazon.smithy.model.shapes.Shape
      ) = TypeChange(
        shape.getType().toString(),
        newModel.expectShape(shape.getId()).getType().toString(),
      )

      override def operationShape(shape: OperationShape): DiffTree = StructChange(
        shape.getId().toString(),
        StructChange(
          "input",
          structureShape(
            shape
              .getInput()
              .get() /* todo */
              .resolved(oldModel)
              .asStructureShape()
              .get()
          ),
        ),
      )

      override def structureShape(shape: StructureShape): DiffTree = shape
        .members()
        .asScala
        .head
        .accept(this)

      override def memberShape(shape: MemberShape): DiffTree = {

        val target = shape.getTarget()
        val newTarget = {
          val newShape = shape.getId.resolved(newModel).asMemberShape().get()

          newShape.getTarget()
        }

        val targetShape = target.resolved(oldModel)
        val newTargetShape = newTarget.resolved(newModel)

        val inner =
          if (newTarget != target) {

            if (
              targetShape.getType().getCategory() == Category.SIMPLE &&
              newTargetShape.getType().getCategory() == Category.SIMPLE
            )
              TypeChange(
                targetShape.getType().toString(),
                newTargetShape.getType().toString(),
              )
            else
              ???
          } else {
            target.resolved(oldModel).accept(this)
          }

        StructChange(shape.getMemberName(), inner)
      }

      override def serviceShape(shape: ServiceShape): DiffTree = StructChange(
        shape.getId().toString(),
        StructChange(
          "operations",
          shape
            .getAllOperations()
            .asScala
            .head /* todo */
            .resolved(oldModel)
            .accept(this),
        ),
      )

    }

  val sc = StructChange(
    "demo#MyService",
    StructChange(
      "operations",
      StructChange(
        "demo#MyOp",
        StructChange(
          "input",
          StructChange(
            "payload",
            StructChange(
              "size",
              TypeChange(
                "integer",
                "string",
              ),
            ),
          ),
        ),
      ),
    ),
  )

  println("expected:")
  println(renderDiffTree(sc, 0))

  oldModel.getServiceShapes().asScala.toList.foreach { service =>
    println(service.accept(toDiffTreeVisitor) == sc)
    println("actual:")
    println(renderDiffTree(service.accept(toDiffTreeVisitor), 0))
  }
}
