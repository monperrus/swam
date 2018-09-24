/*
 * Copyright 2018 Lucas Satabin
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

package swam
package analysis
package traversal

import syntax._

class Traverser {

  def apply(inst: Inst): Unit = inst match {
    case Unop(tpe)   => apply(tpe)
    case Binop(tpe)  => apply(tpe)
    case Testop(tpe) => apply(tpe)
    case Relop(tpe)  => apply(tpe)
    case Convertop(from, to) =>
      apply(from)
      apply(to)
    case Load(tpe, _, _)      => apply(tpe)
    case LoadN(tpe, _, _, _)  => apply(tpe)
    case Store(tpe, _, _)     => apply(tpe)
    case StoreN(tpe, _, _, _) => apply(tpe)
    case Block(tpe, instrs) =>
      apply(tpe)
      instrs.foreach(apply(_))
    case Loop(tpe, instrs) =>
      apply(tpe)
      instrs.foreach(apply(_))
    case If(tpe, ts, es) =>
      apply(tpe)
      ts.foreach(apply(_))
      es.foreach(apply(_))
    case _ => // leaf
  }

  def apply(tpe: Type): Unit = tpe match {
    case ResultType(t) => t.foreach(apply(_))
    case FuncType(params, ret) =>
      params.foreach(apply(_))
      ret.foreach(apply(_))
    case TableType(tpe, _)  => apply(tpe)
    case GlobalType(tpe, _) => apply(tpe)
    case _                  => // leaf
  }

}
