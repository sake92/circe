/*
 * Copyright 2024 circe
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

package io.circe

import io.circe.syntax.*
import io.circe.tests.CirceMunitSuite

class UnionSuite extends CirceMunitSuite:
  import union.given

  case class A(a: Int) derives Codec.AsObject
  case class B(b: String) derives Codec.AsObject
  case class C(c: Boolean) derives Codec.AsObject

  test("Int | String round trip") {
    assertEquals((1: Int | String).asJson, Json.fromInt(1))
    assertEquals(("x": Int | String).asJson, Json.fromString("x"))
    assertEquals(Json.fromInt(1).as[Int | String], Right(1))
    assertEquals(Json.fromString("x").as[Int | String], Right("x"))
  }

  test("case classes, three members") {
    val xs: List[A | B | C] = List(A(1), B("b"), C(true))
    xs.foreach(x => assertEquals(x.asJson.as[A | B | C], Right(x)))
  }

  test("decode order: first success wins") {
    assertEquals(Json.obj("a" -> 1.asJson, "b" -> "s".asJson).as[A | B], Right(A(1)))
  }

  test("failure message") {
    val r = Json.True.as[Int | String]
    assert(r.isLeft)
    assert(r.left.toOption.get.message.contains("union member"))
  }

  test("Option, List, nested unions") {
    assertEquals(Json.fromInt(2).as[Option[Int] | String], Right(Some(2)))
    assertEquals(List(1, 2).asJson.as[List[Int] | String], Right(List(1, 2)))
    assertEquals(("s": (Int | String) | Boolean).asJson, Json.fromString("s"))
  }

  test("codec") {
    val c = summon[Codec[Int | String]]
    assertEquals(c.decodeJson(c(5)), Right(5))
  }

  test("compile errors") {
    assert(compileErrors("summon[Encoder[List[Int] | List[String]]]").nonEmpty)
  }
