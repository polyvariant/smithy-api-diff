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

package org.polyvariant.sad.tests

import org.polyvariant.sad.DiffTree
import org.polyvariant.sad.ToDiffTreeVisitor
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.ShapeId
import weaver._

object DiffTreeTests extends FunSuite {

  def parseModel(text: String): Model = Model
    .assembler()
    .addUnparsedModel(
      "demo.smithy",
      s"""$$version: "2"
       |namespace test
       |$text""".stripMargin,
    )
    .assemble()
    .unwrap()

  /** @param scopeShape
    *   the shape that will be used as the root of the diff tree
    */
  def assertDiff(
    scopeShape: String
  )(
    oldModelText: String,
    newModelText: String,
  )(
    expected: DiffTree
  ) = {
    val oldModel = parseModel(oldModelText)
    val newModel = parseModel(newModelText)

    val scope = oldModel.expectShape(ShapeId.from(scopeShape))

    val diff = scope.accept(new ToDiffTreeVisitor(oldModel, newModel))

    expect(diff == expected)
  }

  test("simple struct member change: only member and type is shown") {
    assertDiff("test#A")(
      """structure A { @required a: String } """,
      """structure A { @required a: Integer } """,
    )(
      DiffTree.TypeChange("string", "integer").inStruct("a")
    )
  }

  test("nested field member change: both members are shown") {
    val structA = "structure A { @required a: B }\n"
    assertDiff("test#A")(
      s"""$structA structure B { @required b: String } """,
      s"""$structA structure B { @required b: Integer } """,
    )(
      DiffTree.TypeChange("string", "integer").inStruct("b").inStruct("a")
    )
  }
}
