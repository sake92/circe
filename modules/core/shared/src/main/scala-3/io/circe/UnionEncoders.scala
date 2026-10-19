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

/**
 * Untagged Scala 3 union type support: encoding picks the first member (in declaration order) whose runtime
 * type matches the value; decoding tries each member in declaration order and returns the first success.
 * Members sharing the same runtime class (e.g. `List[Int] | List[String]`) are rejected at compile time.
 */
private[circe] trait UnionEncoders {
  inline given unionEncoder[A, B]: Encoder[A | B] = ${ UnionMacros.encoder[A | B] }
}
