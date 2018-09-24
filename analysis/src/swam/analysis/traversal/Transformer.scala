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

import cats._

import scala.collection.immutable._

import scala.language.higherKinds

class Transformer {

  private val id: Inst => Inst = (i: Inst) => i

  val i32ConstTransform: i32.Const => Inst = id
  val i32ClzTransform: i32.Clz.type => Inst = id
  val i32CtzTransform: i32.Ctz.type => Inst = id
  val i32PopcntTransform: i32.Popcnt.type => Inst = id
  val i32AddTransform: i32.Add.type => Inst = id
  val i32SubTransform: i32.Sub.type => Inst = id
  val i32MulTransform: i32.Mul.type => Inst = id
  val i32DivSTransform: i32.DivS.type => Inst = id
  val i32DivUTransform: i32.DivU.type => Inst = id
  val i32RemSTransform: i32.RemS.type => Inst = id
  val i32RemUTransform: i32.RemU.type => Inst = id
  val i32AndTransform: i32.And.type => Inst = id
  val i32OrTransform: i32.Or.type => Inst = id
  val i32XorTransform: i32.Xor.type => Inst = id
  val i32ShlTransform: i32.Shl.type => Inst = id
  val i32ShrSTransform: i32.ShrS.type => Inst = id
  val i32ShrUTransform: i32.ShrU.type => Inst = id
  val i32RotlTransform: i32.Rotl.type => Inst = id
  val i32RotrTransform: i32.Rotr.type => Inst = id
  val i32EqzTransform: i32.Eqz.type => Inst = id
  val i32EqTransform: i32.Eq.type => Inst = id
  val i32NeTransform: i32.Ne.type => Inst = id
  val i32LtSTransform: i32.LtS.type => Inst = id
  val i32LtUTransform: i32.LtU.type => Inst = id
  val i32GtSTransform: i32.GtS.type => Inst = id
  val i32GtUTransform: i32.GtU.type => Inst = id
  val i32LeSTransform: i32.LeS.type => Inst = id
  val i32LeUTransform: i32.LeU.type => Inst = id
  val i32GeSTransform: i32.GeS.type => Inst = id
  val i32GeUTransform: i32.GeU.type => Inst = id
  val i32WrapI64Transform: i32.WrapI64.type => Inst = id
  val i32TruncSF32Transform: i32.TruncSF32.type => Inst = id
  val i32TruncUF32Transform: i32.TruncUF32.type => Inst = id
  val i32TruncSF64Transform: i32.TruncSF64.type => Inst = id
  val i32TruncUF64Transform: i32.TruncUF64.type => Inst = id
  val i32ReinterpretF32Transform: i32.ReinterpretF32.type => Inst = id
  val i32LoadTransform: i32.Load => Inst = id
  val i32StoreTransform: i32.Store => Inst = id
  val i32Load8STransform: i32.Load8S => Inst = id
  val i32Load8UTransform: i32.Load8U => Inst = id
  val i32Load16STransform: i32.Load16S => Inst = id
  val i32Load16UTransform: i32.Load16U => Inst = id
  val i32Store8Transform: i32.Store8 => Inst = id
  val i32Store16Transform: i32.Store16 => Inst = id
  val i64ConstTransform: i64.Const => Inst = id
  val i64ClzTransform: i64.Clz.type => Inst = id
  val i64CtzTransform: i64.Ctz.type => Inst = id
  val i64PopcntTransform: i64.Popcnt.type => Inst = id
  val i64AddTransform: i64.Add.type => Inst = id
  val i64SubTransform: i64.Sub.type => Inst = id
  val i64MulTransform: i64.Mul.type => Inst = id
  val i64DivSTransform: i64.DivS.type => Inst = id
  val i64DivUTransform: i64.DivU.type => Inst = id
  val i64RemSTransform: i64.RemS.type => Inst = id
  val i64RemUTransform: i64.RemU.type => Inst = id
  val i64AndTransform: i64.And.type => Inst = id
  val i64OrTransform: i64.Or.type => Inst = id
  val i64XorTransform: i64.Xor.type => Inst = id
  val i64ShlTransform: i64.Shl.type => Inst = id
  val i64ShrSTransform: i64.ShrS.type => Inst = id
  val i64ShrUTransform: i64.ShrU.type => Inst = id
  val i64RotlTransform: i64.Rotl.type => Inst = id
  val i64RotrTransform: i64.Rotr.type => Inst = id
  val i64EqzTransform: i64.Eqz.type => Inst = id
  val i64EqTransform: i64.Eq.type => Inst = id
  val i64NeTransform: i64.Ne.type => Inst = id
  val i64LtSTransform: i64.LtS.type => Inst = id
  val i64LtUTransform: i64.LtU.type => Inst = id
  val i64GtSTransform: i64.GtS.type => Inst = id
  val i64GtUTransform: i64.GtU.type => Inst = id
  val i64LeSTransform: i64.LeS.type => Inst = id
  val i64LeUTransform: i64.LeU.type => Inst = id
  val i64GeSTransform: i64.GeS.type => Inst = id
  val i64GeUTransform: i64.GeU.type => Inst = id
  val i64ExtendSI32Transform: i64.ExtendSI32.type => Inst = id
  val i64ExtendUI32Transform: i64.ExtendUI32.type => Inst = id
  val i64TruncSF32Transform: i64.TruncSF32.type => Inst = id
  val i64TruncUF32Transform: i64.TruncUF32.type => Inst = id
  val i64TruncSF64Transform: i64.TruncSF64.type => Inst = id
  val i64TruncUF64Transform: i64.TruncUF64.type => Inst = id
  val i64ReinterpretF64Transform: i64.ReinterpretF64.type => Inst = id
  val i64LoadTransform: i64.Load => Inst = id
  val i64StoreTransform: i64.Store => Inst = id
  val i64Load8STransform: i64.Load8S => Inst = id
  val i64Load8UTransform: i64.Load8U => Inst = id
  val i64Load16STransform: i64.Load16S => Inst = id
  val i64Load16UTransform: i64.Load16U => Inst = id
  val i64Load32STransform: i64.Load32S => Inst = id
  val i64Load32UTransform: i64.Load32U => Inst = id
  val i64Store8Transform: i64.Store8 => Inst = id
  val i64Store16Transform: i64.Store16 => Inst = id
  val i64Store32Transform: i64.Store32 => Inst = id
  val f32ConstTransform: f32.Const => Inst = id
  val f32AbsTransform: f32.Abs.type => Inst = id
  val f32NegTransform: f32.Neg.type => Inst = id
  val f32SqrtTransform: f32.Sqrt.type => Inst = id
  val f32CeilTransform: f32.Ceil.type => Inst = id
  val f32FloorTransform: f32.Floor.type => Inst = id
  val f32TruncTransform: f32.Trunc.type => Inst = id
  val f32NearestTransform: f32.Nearest.type => Inst = id
  val f32AddTransform: f32.Add.type => Inst = id
  val f32SubTransform: f32.Sub.type => Inst = id
  val f32MulTransform: f32.Mul.type => Inst = id
  val f32DivTransform: f32.Div.type => Inst = id
  val f32MinTransform: f32.Min.type => Inst = id
  val f32MaxTransform: f32.Max.type => Inst = id
  val f32CopysignTransform: f32.Copysign.type => Inst = id
  val f32EqTransform: f32.Eq.type => Inst = id
  val f32NeTransform: f32.Ne.type => Inst = id
  val f32LtTransform: f32.Lt.type => Inst = id
  val f32GtTransform: f32.Gt.type => Inst = id
  val f32LeTransform: f32.Le.type => Inst = id
  val f32GeTransform: f32.Ge.type => Inst = id
  val f32DemoteF64Transform: f32.DemoteF64.type => Inst = id
  val f32ConvertSI32Transform: f32.ConvertSI32.type => Inst = id
  val f32ConvertUI32Transform: f32.ConvertUI32.type => Inst = id
  val f32ConvertSI64Transform: f32.ConvertSI64.type => Inst = id
  val f32ConvertUI64Transform: f32.ConvertUI64.type => Inst = id
  val f32ReinterpretI32Transform: f32.ReinterpretI32.type => Inst = id
  val f32LoadTransform: f32.Load => Inst = id
  val f32StoreTransform: f32.Store => Inst = id
  val f64ConstTransform: f64.Const => Inst = id
  val f64AbsTransform: f64.Abs.type => Inst = id
  val f64NegTransform: f64.Neg.type => Inst = id
  val f64SqrtTransform: f64.Sqrt.type => Inst = id
  val f64CeilTransform: f64.Ceil.type => Inst = id
  val f64FloorTransform: f64.Floor.type => Inst = id
  val f64TruncTransform: f64.Trunc.type => Inst = id
  val f64NearestTransform: f64.Nearest.type => Inst = id
  val f64AddTransform: f64.Add.type => Inst = id
  val f64SubTransform: f64.Sub.type => Inst = id
  val f64MulTransform: f64.Mul.type => Inst = id
  val f64DivTransform: f64.Div.type => Inst = id
  val f64MinTransform: f64.Min.type => Inst = id
  val f64MaxTransform: f64.Max.type => Inst = id
  val f64CopysignTransform: f64.Copysign.type => Inst = id
  val f64EqTransform: f64.Eq.type => Inst = id
  val f64NeTransform: f64.Ne.type => Inst = id
  val f64LtTransform: f64.Lt.type => Inst = id
  val f64GtTransform: f64.Gt.type => Inst = id
  val f64LeTransform: f64.Le.type => Inst = id
  val f64GeTransform: f64.Ge.type => Inst = id
  val f64PromoteF32Transform: f64.PromoteF32.type => Inst = id
  val f64ConvertSI32Transform: f64.ConvertSI32.type => Inst = id
  val f64ConvertUI32Transform: f64.ConvertUI32.type => Inst = id
  val f64ConvertSI64Transform: f64.ConvertSI64.type => Inst = id
  val f64ConvertUI64Transform: f64.ConvertUI64.type => Inst = id
  val f64ReinterpretI64Transform: f64.ReinterpretI64.type => Inst = id
  val f64LoadTransform: f64.Load => Inst = id
  val f64StoreTransform: f64.Store => Inst = id
  val DropTransform: Drop.type => Inst = id
  val SelectTransform: Select.type => Inst = id
  val LocalGetTransform: LocalGet => Inst = id
  val LocalSetTransform: LocalSet => Inst = id
  val LocalTeeTransform: LocalTee => Inst = id
  val GlobalGetTransform: GlobalGet => Inst = id
  val GlobalSetTransform: GlobalSet => Inst = id
  val MemorySizeTransform: MemorySize.type => Inst = id
  val MemoryGrowTransform: MemoryGrow.type => Inst = id
  val NopTransform: Nop.type => Inst = id
  val UnreachableTransform: Unreachable.type => Inst = id
  val BlockTransform: Block => Inst = id
  val LoopTransform: Loop => Inst = id
  val IfTransform: If => Inst = id
  val BrTransform: Br => Inst = id
  val BrIfTransform: BrIf => Inst = id
  val BrTableTransform: BrTable => Inst = id
  val ReturnTransform: Return.type => Inst = id
  val CallTransform: Call => Inst = id
  val CallIndirectTransform: CallIndirect => Inst = id

  final def transform(inst: Inst): Inst = inst match {
    case inst: i32.Const               => i32ConstTransform(inst)
    case inst: i32.Clz.type            => i32ClzTransform(inst)
    case inst: i32.Ctz.type            => i32CtzTransform(inst)
    case inst: i32.Popcnt.type         => i32PopcntTransform(inst)
    case inst: i32.Add.type            => i32AddTransform(inst)
    case inst: i32.Sub.type            => i32SubTransform(inst)
    case inst: i32.Mul.type            => i32MulTransform(inst)
    case inst: i32.DivS.type           => i32DivSTransform(inst)
    case inst: i32.DivU.type           => i32DivUTransform(inst)
    case inst: i32.RemS.type           => i32RemSTransform(inst)
    case inst: i32.RemU.type           => i32RemUTransform(inst)
    case inst: i32.And.type            => i32AndTransform(inst)
    case inst: i32.Or.type             => i32OrTransform(inst)
    case inst: i32.Xor.type            => i32XorTransform(inst)
    case inst: i32.Shl.type            => i32ShlTransform(inst)
    case inst: i32.ShrS.type           => i32ShrSTransform(inst)
    case inst: i32.ShrU.type           => i32ShrUTransform(inst)
    case inst: i32.Rotl.type           => i32RotlTransform(inst)
    case inst: i32.Rotr.type           => i32RotrTransform(inst)
    case inst: i32.Eqz.type            => i32EqzTransform(inst)
    case inst: i32.Eq.type             => i32EqTransform(inst)
    case inst: i32.Ne.type             => i32NeTransform(inst)
    case inst: i32.LtS.type            => i32LtSTransform(inst)
    case inst: i32.LtU.type            => i32LtUTransform(inst)
    case inst: i32.GtS.type            => i32GtSTransform(inst)
    case inst: i32.GtU.type            => i32GtUTransform(inst)
    case inst: i32.LeS.type            => i32LeSTransform(inst)
    case inst: i32.LeU.type            => i32LeUTransform(inst)
    case inst: i32.GeS.type            => i32GeSTransform(inst)
    case inst: i32.GeU.type            => i32GeUTransform(inst)
    case inst: i32.WrapI64.type        => i32WrapI64Transform(inst)
    case inst: i32.TruncSF32.type      => i32TruncSF32Transform(inst)
    case inst: i32.TruncUF32.type      => i32TruncUF32Transform(inst)
    case inst: i32.TruncSF64.type      => i32TruncSF64Transform(inst)
    case inst: i32.TruncUF64.type      => i32TruncUF64Transform(inst)
    case inst: i32.ReinterpretF32.type => i32ReinterpretF32Transform(inst)
    case inst: i32.Load                => i32LoadTransform(inst)
    case inst: i32.Store               => i32StoreTransform(inst)
    case inst: i32.Load8S              => i32Load8STransform(inst)
    case inst: i32.Load8U              => i32Load8UTransform(inst)
    case inst: i32.Load16S             => i32Load16STransform(inst)
    case inst: i32.Load16U             => i32Load16UTransform(inst)
    case inst: i32.Store8              => i32Store8Transform(inst)
    case inst: i32.Store16             => i32Store16Transform(inst)
    case inst: i64.Const               => i64ConstTransform(inst)
    case inst: i64.Clz.type            => i64ClzTransform(inst)
    case inst: i64.Ctz.type            => i64CtzTransform(inst)
    case inst: i64.Popcnt.type         => i64PopcntTransform(inst)
    case inst: i64.Add.type            => i64AddTransform(inst)
    case inst: i64.Sub.type            => i64SubTransform(inst)
    case inst: i64.Mul.type            => i64MulTransform(inst)
    case inst: i64.DivS.type           => i64DivSTransform(inst)
    case inst: i64.DivU.type           => i64DivUTransform(inst)
    case inst: i64.RemS.type           => i64RemSTransform(inst)
    case inst: i64.RemU.type           => i64RemUTransform(inst)
    case inst: i64.And.type            => i64AndTransform(inst)
    case inst: i64.Or.type             => i64OrTransform(inst)
    case inst: i64.Xor.type            => i64XorTransform(inst)
    case inst: i64.Shl.type            => i64ShlTransform(inst)
    case inst: i64.ShrS.type           => i64ShrSTransform(inst)
    case inst: i64.ShrU.type           => i64ShrUTransform(inst)
    case inst: i64.Rotl.type           => i64RotlTransform(inst)
    case inst: i64.Rotr.type           => i64RotrTransform(inst)
    case inst: i64.Eqz.type            => i64EqzTransform(inst)
    case inst: i64.Eq.type             => i64EqTransform(inst)
    case inst: i64.Ne.type             => i64NeTransform(inst)
    case inst: i64.LtS.type            => i64LtSTransform(inst)
    case inst: i64.LtU.type            => i64LtUTransform(inst)
    case inst: i64.GtS.type            => i64GtSTransform(inst)
    case inst: i64.GtU.type            => i64GtUTransform(inst)
    case inst: i64.LeS.type            => i64LeSTransform(inst)
    case inst: i64.LeU.type            => i64LeUTransform(inst)
    case inst: i64.GeS.type            => i64GeSTransform(inst)
    case inst: i64.GeU.type            => i64GeUTransform(inst)
    case inst: i64.ExtendSI32.type     => i64ExtendSI32Transform(inst)
    case inst: i64.ExtendUI32.type     => i64ExtendUI32Transform(inst)
    case inst: i64.TruncSF32.type      => i64TruncSF32Transform(inst)
    case inst: i64.TruncUF32.type      => i64TruncUF32Transform(inst)
    case inst: i64.TruncSF64.type      => i64TruncSF64Transform(inst)
    case inst: i64.TruncUF64.type      => i64TruncUF64Transform(inst)
    case inst: i64.ReinterpretF64.type => i64ReinterpretF64Transform(inst)
    case inst: i64.Load                => i64LoadTransform(inst)
    case inst: i64.Store               => i64StoreTransform(inst)
    case inst: i64.Load8S              => i64Load8STransform(inst)
    case inst: i64.Load8U              => i64Load8UTransform(inst)
    case inst: i64.Load16S             => i64Load16STransform(inst)
    case inst: i64.Load16U             => i64Load16UTransform(inst)
    case inst: i64.Load32S             => i64Load32STransform(inst)
    case inst: i64.Load32U             => i64Load32UTransform(inst)
    case inst: i64.Store8              => i64Store8Transform(inst)
    case inst: i64.Store16             => i64Store16Transform(inst)
    case inst: i64.Store32             => i64Store32Transform(inst)
    case inst: f32.Const               => f32ConstTransform(inst)
    case inst: f32.Abs.type            => f32AbsTransform(inst)
    case inst: f32.Neg.type            => f32NegTransform(inst)
    case inst: f32.Sqrt.type           => f32SqrtTransform(inst)
    case inst: f32.Ceil.type           => f32CeilTransform(inst)
    case inst: f32.Floor.type          => f32FloorTransform(inst)
    case inst: f32.Trunc.type          => f32TruncTransform(inst)
    case inst: f32.Nearest.type        => f32NearestTransform(inst)
    case inst: f32.Add.type            => f32AddTransform(inst)
    case inst: f32.Sub.type            => f32SubTransform(inst)
    case inst: f32.Mul.type            => f32MulTransform(inst)
    case inst: f32.Div.type            => f32DivTransform(inst)
    case inst: f32.Min.type            => f32MinTransform(inst)
    case inst: f32.Max.type            => f32MaxTransform(inst)
    case inst: f32.Copysign.type       => f32CopysignTransform(inst)
    case inst: f32.Eq.type             => f32EqTransform(inst)
    case inst: f32.Ne.type             => f32NeTransform(inst)
    case inst: f32.Lt.type             => f32LtTransform(inst)
    case inst: f32.Gt.type             => f32GtTransform(inst)
    case inst: f32.Le.type             => f32LeTransform(inst)
    case inst: f32.Ge.type             => f32GeTransform(inst)
    case inst: f32.DemoteF64.type      => f32DemoteF64Transform(inst)
    case inst: f32.ConvertSI32.type    => f32ConvertSI32Transform(inst)
    case inst: f32.ConvertUI32.type    => f32ConvertUI32Transform(inst)
    case inst: f32.ConvertSI64.type    => f32ConvertSI64Transform(inst)
    case inst: f32.ConvertUI64.type    => f32ConvertUI64Transform(inst)
    case inst: f32.ReinterpretI32.type => f32ReinterpretI32Transform(inst)
    case inst: f32.Load                => f32LoadTransform(inst)
    case inst: f32.Store               => f32StoreTransform(inst)
    case inst: f64.Const               => f64ConstTransform(inst)
    case inst: f64.Abs.type            => f64AbsTransform(inst)
    case inst: f64.Neg.type            => f64NegTransform(inst)
    case inst: f64.Sqrt.type           => f64SqrtTransform(inst)
    case inst: f64.Ceil.type           => f64CeilTransform(inst)
    case inst: f64.Floor.type          => f64FloorTransform(inst)
    case inst: f64.Trunc.type          => f64TruncTransform(inst)
    case inst: f64.Nearest.type        => f64NearestTransform(inst)
    case inst: f64.Add.type            => f64AddTransform(inst)
    case inst: f64.Sub.type            => f64SubTransform(inst)
    case inst: f64.Mul.type            => f64MulTransform(inst)
    case inst: f64.Div.type            => f64DivTransform(inst)
    case inst: f64.Min.type            => f64MinTransform(inst)
    case inst: f64.Max.type            => f64MaxTransform(inst)
    case inst: f64.Copysign.type       => f64CopysignTransform(inst)
    case inst: f64.Eq.type             => f64EqTransform(inst)
    case inst: f64.Ne.type             => f64NeTransform(inst)
    case inst: f64.Lt.type             => f64LtTransform(inst)
    case inst: f64.Gt.type             => f64GtTransform(inst)
    case inst: f64.Le.type             => f64LeTransform(inst)
    case inst: f64.Ge.type             => f64GeTransform(inst)
    case inst: f64.PromoteF32.type     => f64PromoteF32Transform(inst)
    case inst: f64.ConvertSI32.type    => f64ConvertSI32Transform(inst)
    case inst: f64.ConvertUI32.type    => f64ConvertUI32Transform(inst)
    case inst: f64.ConvertSI64.type    => f64ConvertSI64Transform(inst)
    case inst: f64.ConvertUI64.type    => f64ConvertUI64Transform(inst)
    case inst: f64.ReinterpretI64.type => f64ReinterpretI64Transform(inst)
    case inst: f64.Load                => f64LoadTransform(inst)
    case inst: f64.Store               => f64StoreTransform(inst)
    case inst: Drop.type               => DropTransform(inst)
    case inst: Select.type             => SelectTransform(inst)
    case inst: LocalGet                => LocalGetTransform(inst)
    case inst: LocalSet                => LocalSetTransform(inst)
    case inst: LocalTee                => LocalTeeTransform(inst)
    case inst: GlobalGet               => GlobalGetTransform(inst)
    case inst: GlobalSet               => GlobalSetTransform(inst)
    case inst: MemorySize.type         => MemorySizeTransform(inst)
    case inst: MemoryGrow.type         => MemoryGrowTransform(inst)
    case inst: Nop.type                => NopTransform(inst)
    case inst: Unreachable.type        => UnreachableTransform(inst)
    case inst: Block                   => BlockTransform(inst)
    case inst: Loop                    => LoopTransform(inst)
    case inst: If                      => IfTransform(inst)
    case inst: Br                      => BrTransform(inst)
    case inst: BrIf                    => BrIfTransform(inst)
    case inst: BrTable                 => BrTableTransform(inst)
    case inst: Return.type             => ReturnTransform(inst)
    case inst: Call                    => CallTransform(inst)
    case inst: CallIndirect            => CallIndirectTransform(inst)
  }

  final def run(inst: Inst): Inst =
    inst match {
      case Block(tpe, is)  => transform(Block(tpe, is.map(transform(_))))
      case Loop(tpe, is)   => transform(Loop(tpe, is.map(transform(_))))
      case If(tpe, ts, es) => transform(If(tpe, ts.map(transform(_)), es.map(transform(_))))
      case _               => transform(inst)
    }

}

object Transformer {}
