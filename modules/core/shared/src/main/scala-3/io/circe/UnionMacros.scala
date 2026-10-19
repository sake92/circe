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

import scala.quoted.*

private[circe] object UnionMacros:
  def members[T: Type](using q: Quotes): List[q.reflect.TypeRepr] =
    import quotes.reflect.*
    def go(t: TypeRepr): List[TypeRepr] = t.dealias match
      case OrType(l, r) => go(l) ::: go(r)
      case other        => List(other)
    val ms = go(TypeRepr.of[T]).distinct
    val erased = ms.map(_.dealias.widen.typeSymbol)
    erased.diff(erased.distinct).headOption.foreach { s =>
      report.errorAndAbort(
        s"Cannot derive a codec for ${Type.show[T]}: multiple union members have the same runtime class ${s.name}"
      )
    }
    ms

  def encoder[T: Type](using Quotes): Expr[Encoder[T]] =
    import quotes.reflect.*
    val cases: List[Expr[(Any => Boolean, Encoder[Any])]] = members[T].map { m =>
      m.asType match
        case '[t] =>
          val enc = Expr.summon[Encoder[t]].getOrElse(
            report.errorAndAbort(s"No Encoder found for union member ${Type.show[t]}")
          )
          '{ ((a: Any) => a.isInstanceOf[t @unchecked], $enc.asInstanceOf[Encoder[Any]]) }
    }
    val list = Expr.ofList(cases)
    '{
      val cs = $list
      new Encoder[T]:
        final def apply(a: T): Json = cs.find(_._1(a)) match
          case Some((_, enc)) => enc(a)
          case None           => throw new IllegalArgumentException(s"No union member matches value: $a")
    }

  def decoder[T: Type](using Quotes): Expr[Decoder[T]] =
    import quotes.reflect.*
    val cases: List[Expr[Decoder[Any]]] = members[T].map { m =>
      m.asType match
        case '[t] =>
          val dec = Expr.summon[Decoder[t]].getOrElse(
            report.errorAndAbort(s"No Decoder found for union member ${Type.show[t]}")
          )
          '{ $dec.asInstanceOf[Decoder[Any]] }
    }
    val names = Expr(members[T].map(_.show))
    val list = Expr.ofList(cases)
    '{
      val ds = $list
      val ns = $names
      new Decoder[T]:
        final def apply(c: HCursor): Decoder.Result[T] =
          var errors = List.empty[String]
          val it = ds.iterator.zip(ns.iterator)
          while it.hasNext do
            val (d, n) = it.next()
            d(c) match
              case Right(a) => return Right(a.asInstanceOf[T])
              case Left(e)  => errors = s"$n: ${e.message}" :: errors
          Left(DecodingFailure(s"Could not decode as any union member (${errors.reverse.mkString("; ")})", c.history))
    }
  