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
package runtime
package internals
package interpreter

import instance._

import java.lang.{Integer => JInt, Long => JLong, Float => JFloat, Double => JDouble}

import scala.annotation.{tailrec, switch}

import java.nio.ByteBuffer

import cats._
import cats.implicits._

import scala.language.higherKinds

/** An interpreter is spwan each time the execution of a method is required. */
private[runtime] class Interpreter[F[_]](engine: SwamEngine[F])(implicit F: MonadError[F, Throwable]) {

  private val conf = engine.conf

  def interpret(funcidx: Int, parameters: Vector[Value], instance: Instance[F]): F[Option[Value]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // push the parameters in the stack
    frame.stack.pushValues(parameters)
    // invoke the function
    invoke(frame, instance.function(funcidx)).flatMap {
      case Left(frame) => run(frame)
      case Right(res) => F.pure(res)
    }
  }

  def interpret(func: Function[F], parameters: Vector[Value], instance: Instance[F]): F[Option[Value]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // push the parameters in the stack
    frame.stack.pushValues(parameters)
    // invoke the function
    invoke(frame, func).flatMap {
      case Left(frame) => run(frame)
      case Right(res) => F.pure(res)
    }
  }

  def interpretInit(tpe: ValType, code: ByteBuffer, instance: Instance[F]): F[Option[Value]] = {
    // instantiate the top-level frame
    val frame = Frame.makeToplevel[F](instance, conf)
    // invoke the function
    invoke(frame, new FunctionInstance(FuncType(Vector(), Vector(tpe)), Vector(), code, instance)).flatMap {
      case Left(frame) => run(frame)
      case Right(res) => F.pure(res)
    }
  }

  private def run(frame: Frame[F]): F[Option[Value]] =
    F.tailRecM(frame) { frame =>
      val opcode = frame.readByte() & 0xff
      (opcode: @switch) match {
        // === constants ===
        case OpCode.I32Const =>
          frame.stack.pushInt(frame.readInt())
          F.pure(Left(frame))
        case OpCode.I64Const =>
          frame.stack.pushLong(frame.readLong())
          F.pure(Left(frame))
        case OpCode.F32Const =>
          frame.stack.pushFloat(frame.readFloat())
          F.pure(Left(frame))
        case OpCode.F64Const =>
          frame.stack.pushDouble(frame.readDouble())
          F.pure(Left(frame))
        // === unary operators ===
        case OpCode.I32Clz =>
          frame.stack.pushInt(JInt.numberOfLeadingZeros(frame.stack.popInt()))
          F.pure(Left(frame))
        case OpCode.I32Ctz =>
          frame.stack.pushInt(JInt.numberOfTrailingZeros(frame.stack.popInt()))
          F.pure(Left(frame))
        case OpCode.I32Popcnt =>
          frame.stack.pushInt(JInt.bitCount(frame.stack.popInt()))
          F.pure(Left(frame))
        case OpCode.I64Clz =>
          frame.stack.pushLong(JLong.numberOfLeadingZeros(frame.stack.popLong()))
          F.pure(Left(frame))
        case OpCode.I64Ctz =>
          frame.stack.pushLong(JLong.numberOfTrailingZeros(frame.stack.popLong()))
          F.pure(Left(frame))
        case OpCode.I64Popcnt =>
          frame.stack.pushLong(JLong.bitCount(frame.stack.popLong()))
          F.pure(Left(frame))
        case OpCode.F32Abs =>
          frame.stack.pushFloat(JFloat.intBitsToFloat(JFloat.floatToRawIntBits(frame.stack.popFloat()) & 0x7fffffff))
          F.pure(Left(frame))
        case OpCode.F32Neg =>
          frame.stack.pushFloat(-frame.stack.popFloat())
          F.pure(Left(frame))
        case OpCode.F32Sqrt =>
          frame.stack.pushFloat(StrictMath.sqrt(frame.stack.popFloat()).toFloat)
          F.pure(Left(frame))
        case OpCode.F32Ceil =>
          frame.stack.pushFloat(frame.stack.popFloat().ceil)
          F.pure(Left(frame))
        case OpCode.F32Floor =>
          frame.stack.pushFloat(frame.stack.popFloat().floor)
          F.pure(Left(frame))
        case OpCode.F32Trunc =>
          val f = frame.stack.popFloat()
          frame.stack.pushFloat(F32.trunc(f))
          F.pure(Left(frame))
        case OpCode.F32Nearest =>
          val f = frame.stack.popFloat()
          frame.stack.pushFloat(F32.nearest(f))
          F.pure(Left(frame))
        case OpCode.F64Abs =>
          frame.stack.pushDouble(JDouble.longBitsToDouble(JDouble.doubleToRawLongBits(frame.stack.popDouble()) & 0x7fffffffffffffffl))
          F.pure(Left(frame))
        case OpCode.F64Neg =>
          frame.stack.pushDouble(-frame.stack.popDouble())
          F.pure(Left(frame))
        case OpCode.F64Sqrt =>
          frame.stack.pushDouble(StrictMath.sqrt(frame.stack.popDouble()))
          F.pure(Left(frame))
        case OpCode.F64Ceil =>
          frame.stack.pushDouble(frame.stack.popDouble().ceil)
          F.pure(Left(frame))
        case OpCode.F64Floor =>
          frame.stack.pushDouble(frame.stack.popDouble().floor)
          F.pure(Left(frame))
        case OpCode.F64Trunc =>
          val f = frame.stack.popDouble()
          frame.stack.pushDouble(F64.trunc(f))
          F.pure(Left(frame))
        case OpCode.F64Nearest =>
          val d = frame.stack.popDouble()
          frame.stack.pushDouble(F64.nearest(d))
          F.pure(Left(frame))
        // === binary operators ===
        case OpCode.I32Add =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 + i2)
          F.pure(Left(frame))
        case OpCode.I32Sub =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 - i2)
          F.pure(Left(frame))
        case OpCode.I32Mul =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 * i2)
          F.pure(Left(frame))
        case OpCode.I32DivU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(JInt.divideUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case OpCode.I32DivS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new InterpreterException(frame, "overflow"))
            } else {
              frame.stack.pushInt(i1 / i2)
              F.pure(Left(frame))
            }
          }
        case OpCode.I32RemU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(JInt.remainderUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case OpCode.I32RemS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushInt(i1 % i2)
            F.pure(Left(frame))
          }
        case OpCode.I32And =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 & i2)
          F.pure(Left(frame))
        case OpCode.I32Or =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 | i2)
          F.pure(Left(frame))
        case OpCode.I32Xor =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 ^ i2)
          F.pure(Left(frame))
        case OpCode.I32Shl =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 << i2)
          F.pure(Left(frame))
        case OpCode.I32ShrU =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 >>> i2)
          F.pure(Left(frame))
        case OpCode.I32ShrS =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(i1 >> i2)
          F.pure(Left(frame))
        case OpCode.I32Rotl =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(JInt.rotateLeft(i1, i2))
          F.pure(Left(frame))
        case OpCode.I32Rotr =>
          val i2 = frame.stack.popInt() % 32
          val i1 = frame.stack.popInt()
          frame.stack.pushInt(JInt.rotateRight(i1, i2))
          F.pure(Left(frame))
        case OpCode.I64Add =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 + i2)
          F.pure(Left(frame))
        case OpCode.I64Sub =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 - i2)
          F.pure(Left(frame))
        case OpCode.I64Mul =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 * i2)
          F.pure(Left(frame))
        case OpCode.I64DivU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(JLong.divideUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case OpCode.I64DivS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            val res = i1 / i2
            if (i1 >= 0 && i2 > 0 && res < 0) {
              F.raiseError(new InterpreterException(frame, "overflow"))
            } else {
              frame.stack.pushLong(i1 / i2)
              F.pure(Left(frame))
            }
          }
        case OpCode.I64RemU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(JLong.remainderUnsigned(i1, i2))
            F.pure(Left(frame))
          }
        case OpCode.I64RemS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          if (i2 == 0) {
            F.raiseError(new InterpreterException(frame, "integer divide by zero"))
          } else {
            frame.stack.pushLong(i1 % i2)
            F.pure(Left(frame))
          }
        case OpCode.I64And =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 & i2)
          F.pure(Left(frame))
        case OpCode.I64Or =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 | i2)
          F.pure(Left(frame))
        case OpCode.I64Xor =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 ^ i2)
          F.pure(Left(frame))
        case OpCode.I64Shl =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 << i2)
          F.pure(Left(frame))
        case OpCode.I64ShrU =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 >>> i2)
          F.pure(Left(frame))
        case OpCode.I64ShrS =>
          val i2 = frame.stack.popLong() % 64
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(i1 >> i2)
          F.pure(Left(frame))
        case OpCode.I64Rotl =>
          val i2 = (frame.stack.popLong() % 64).toInt
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(JLong.rotateLeft(i1, i2))
          F.pure(Left(frame))
        case OpCode.I64Rotr =>
          val i2 = (frame.stack.popLong() % 64).toInt
          val i1 = frame.stack.popLong()
          frame.stack.pushLong(JLong.rotateRight(i1, i2))
          F.pure(Left(frame))
        case OpCode.F32Add =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 + f2)
          F.pure(Left(frame))
        case OpCode.F32Sub =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 - f2)
          F.pure(Left(frame))
        case OpCode.F32Mul =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 * f2)
          F.pure(Left(frame))
        case OpCode.F32Div =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(f1 / f2)
          F.pure(Left(frame))
        case OpCode.F32Min =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(StrictMath.min(f1, f2))
          F.pure(Left(frame))
        case OpCode.F32Max =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(StrictMath.max(f1, f2))
          F.pure(Left(frame))
        case OpCode.F32Copysign =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushFloat(Math.copySign(f1, f2))
          F.pure(Left(frame))
        case OpCode.F64Add =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 + f2)
          F.pure(Left(frame))
        case OpCode.F64Sub =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 - f2)
          F.pure(Left(frame))
        case OpCode.F64Mul =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 * f2)
          F.pure(Left(frame))
        case OpCode.F64Div =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(f1 / f2)
          F.pure(Left(frame))
        case OpCode.F64Min =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(StrictMath.min(f1, f2))
          F.pure(Left(frame))
        case OpCode.F64Max =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(StrictMath.max(f1, f2))
          F.pure(Left(frame))
        case OpCode.F64Copysign =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushDouble(Math.copySign(f1, f2))
          F.pure(Left(frame))
        // === test operators ===
        case OpCode.I32Eqz =>
          val i = frame.stack.popInt()
          frame.stack.pushBool(i == 0)
          F.pure(Left(frame))
        case OpCode.I64Eqz =>
          val i = frame.stack.popLong()
          frame.stack.pushBool(i == 0)
          F.pure(Left(frame))
        // === relation operators ===
        case OpCode.I32Eq =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 == i2)
          F.pure(Left(frame))
        case OpCode.I32Ne =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 != i2)
          F.pure(Left(frame))
        case OpCode.I32LtU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) < 0)
          F.pure(Left(frame))
        case OpCode.I32LtS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 < i2)
          F.pure(Left(frame))
        case OpCode.I32GtU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) > 0)
          F.pure(Left(frame))
        case OpCode.I32GtS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 > i2)
          F.pure(Left(frame))
        case OpCode.I32LeU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) <= 0)
          F.pure(Left(frame))
        case OpCode.I32LeS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 <= i2)
          F.pure(Left(frame))
        case OpCode.I32GeU =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(JInt.compareUnsigned(i1, i2) >= 0)
          F.pure(Left(frame))
        case OpCode.I32GeS =>
          val i2 = frame.stack.popInt()
          val i1 = frame.stack.popInt()
          frame.stack.pushBool(i1 >= i2)
          F.pure(Left(frame))
        case OpCode.I64Eq =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 == i2)
          F.pure(Left(frame))
        case OpCode.I64Ne =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 != i2)
          F.pure(Left(frame))
        case OpCode.I64LtU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) < 0)
          F.pure(Left(frame))
        case OpCode.I64LtS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 < i2)
          F.pure(Left(frame))
        case OpCode.I64GtU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) > 0)
          F.pure(Left(frame))
        case OpCode.I64GtS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 > i2)
          F.pure(Left(frame))
        case OpCode.I64LeU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) <= 0)
          F.pure(Left(frame))
        case OpCode.I64LeS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 <= i2)
          F.pure(Left(frame))
        case OpCode.I64GeU =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(JLong.compareUnsigned(i1, i2) >= 0)
          F.pure(Left(frame))
        case OpCode.I64GeS =>
          val i2 = frame.stack.popLong()
          val i1 = frame.stack.popLong()
          frame.stack.pushBool(i1 >= i2)
          F.pure(Left(frame))
        case OpCode.F32Eq =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 == f2)
          F.pure(Left(frame))
        case OpCode.F32Ne =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 != f2)
          F.pure(Left(frame))
        case OpCode.F32Lt =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 < f2)
          F.pure(Left(frame))
        case OpCode.F32Gt =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 > f2)
          F.pure(Left(frame))
        case OpCode.F32Le =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 <= f2)
          F.pure(Left(frame))
        case OpCode.F32Ge =>
          val f2 = frame.stack.popFloat()
          val f1 = frame.stack.popFloat()
          frame.stack.pushBool(f1 >= f2)
          F.pure(Left(frame))
        case OpCode.F64Eq =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 == f2)
          F.pure(Left(frame))
        case OpCode.F64Ne =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 != f2)
          F.pure(Left(frame))
        case OpCode.F64Lt =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 < f2)
          F.pure(Left(frame))
        case OpCode.F64Gt =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 > f2)
          F.pure(Left(frame))
        case OpCode.F64Le =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 <= f2)
          F.pure(Left(frame))
        case OpCode.F64Ge =>
          val f2 = frame.stack.popDouble()
          val f1 = frame.stack.popDouble()
          frame.stack.pushBool(f1 >= f2)
          F.pure(Left(frame))
        // === conversion operators ===
        case OpCode.I32WrapI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushInt(I32.wrap(l))
          F.pure(Left(frame))
        case OpCode.I64ExtendUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushLong(I64.extendUi32(i))
          F.pure(Left(frame))
        case OpCode.I64ExtendSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushLong(I64.extendSi32(i))
          F.pure(Left(frame))
        case OpCode.I32TruncUF32 =>
          val f = frame.stack.popFloat()
          F.catchNonFatal(I32.truncUf32(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { i =>
              frame.stack.pushInt(i)
              Left(frame)
            }
        case OpCode.I32TruncSF32 =>
          val f = frame.stack.popFloat()
          F.catchNonFatal(I32.truncSf32(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { i =>
              frame.stack.pushInt(i)
              Left(frame)
            }
        case OpCode.I32TruncUF64 =>
          val f = frame.stack.popDouble()
          F.catchNonFatal(I32.truncUf64(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { i =>
              frame.stack.pushInt(i)
              Left(frame)
            }
        case OpCode.I32TruncSF64 =>
          val f = frame.stack.popDouble()
          F.catchNonFatal(I32.truncSf64(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { i =>
              frame.stack.pushInt(i)
              Left(frame)
            }
        case OpCode.I64TruncUF32 =>
          val f = frame.stack.popFloat()
          F.catchNonFatal(I64.truncUf32(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { l =>
              frame.stack.pushLong(l)
              Left(frame)
            }
        case OpCode.I64TruncSF32 =>
          val f = frame.stack.popFloat()
          F.catchNonFatal(I64.truncSf32(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { l =>
              frame.stack.pushLong(l)
              Left(frame)
            }
        case OpCode.I64TruncUF64 =>
          val f = frame.stack.popDouble()
          F.catchNonFatal(I64.truncUf64(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { l =>
              frame.stack.pushLong(l)
              Left(frame)
            }
        case OpCode.I64TruncSF64 =>
          val f = frame.stack.popDouble()
          F.catchNonFatal(I64.truncSf64(f)).adaptError {
            case e => new InterpreterException(frame, e.getMessage)
            }.map { l =>
              frame.stack.pushLong(l)
              Left(frame)
            }
        case OpCode.F32DemoteF64 =>
          val f = frame.stack.popDouble()
          frame.stack.pushFloat(F32.demote(f))
          F.pure(Left(frame))
        case OpCode.F64PromoteF32 =>
          val f = frame.stack.popFloat()
          frame.stack.pushDouble(F64.promote(f))
          F.pure(Left(frame))
        case OpCode.F32ConvertUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.convertUi32(i))
          F.pure(Left(frame))
        case OpCode.F32ConvertSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.convertSi32(i))
          F.pure(Left(frame))
        case OpCode.F32ConvertUI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushFloat(F32.convertUi64(l))
          F.pure(Left(frame))
        case OpCode.F32ConvertSI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushFloat(F32.convertSi64(l))
          F.pure(Left(frame))
        case OpCode.F64ConvertUI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushDouble(F64.convertUi32(i))
          F.pure(Left(frame))
        case OpCode.F64ConvertSI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushDouble(F64.convertSi32(i))
          F.pure(Left(frame))
        case OpCode.F64ConvertUI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushDouble(F64.convertUi64(l))
          F.pure(Left(frame))
        case OpCode.F64ConvertSI64 =>
          val l = frame.stack.popLong()
          frame.stack.pushDouble(F64.convertSi64(l))
          F.pure(Left(frame))
        case OpCode.I32ReinterpretF32 =>
          val f = frame.stack.popFloat()
          frame.stack.pushInt(I32.reinterpret(f))
          F.pure(Left(frame))
        case OpCode.I64ReinterpretF64 =>
          val f = frame.stack.popDouble()
          frame.stack.pushLong(I64.reinterpret(f))
          F.pure(Left(frame))
        case OpCode.F32ReinterpretI32 =>
          val i = frame.stack.popInt()
          frame.stack.pushFloat(F32.reinterpret(i))
          F.pure(Left(frame))
        case OpCode.F64ReinterpretI64 =>
          val i = frame.stack.popLong()
          frame.stack.pushDouble(F64.reinterpret(i))
          F.pure(Left(frame))
        // === parameteric instructions ===
        case OpCode.Drop =>
          frame.stack.drop()
          F.pure(Left(frame))
        case OpCode.Select =>
          val b = frame.stack.popBool()
          val v2 = frame.stack.popValue()
          val v1 = frame.stack.popValue()
          if (b)
            frame.stack.pushValue(v1)
          else
            frame.stack.pushValue(v2)
          F.pure(Left(frame))
        case OpCode.GetLocal =>
          val idx = frame.readInt()
          frame.stack.pushValue(frame.locals(idx))
          F.pure(Left(frame))
        case OpCode.SetLocal =>
          val idx = frame.readInt()
          val v = frame.stack.popValue()
          frame.locals(idx) = v
          F.pure(Left(frame))
        case OpCode.TeeLocal =>
          val idx = frame.readInt()
          val v = frame.stack.peekValue()
          frame.locals(idx) = v
          F.pure(Left(frame))
        case OpCode.GetGlobal =>
          val idx = frame.readInt()
          frame.stack.pushValue(frame.instance.global(idx))
          F.pure(Left(frame))
        case OpCode.SetGlobal =>
          val idx = frame.readInt()
          val v = frame.stack.popValue()
          (frame.instance.global(idx) = v).map(_ => Left(frame))
        // === memory instructions ===
        case OpCode.I32Load =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readInt(ea)
            frame.stack.pushInt(c)
            F.pure(Left(frame))
          }
        case OpCode.I32Load8U =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readByte(ea) & 0xff
            frame.stack.pushInt(c)
            F.pure(Left(frame))
          }
        case OpCode.I32Load8S =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readByte(ea)
            frame.stack.pushInt(c)
            F.pure(Left(frame))
          }
        case OpCode.I32Load16U =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readShort(ea)
            frame.stack.pushInt(c & 0xffff)
            F.pure(Left(frame))
          }
        case OpCode.I32Load16S =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readShort(ea)
            frame.stack.pushInt(c)
            F.pure(Left(frame))
          }
        case OpCode.I64Load =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readLong(ea)
            frame.stack.pushLong(c)
            F.pure(Left(frame))
          }
        case OpCode.I64Load8U =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readByte(ea)
            frame.stack.pushLong(c & 0xffl)
            F.pure(Left(frame))
          }
        case OpCode.I64Load8S =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readByte(ea)
            frame.stack.pushLong(c)
            F.pure(Left(frame))
          }
        case OpCode.I64Load16U =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readShort(ea)
            frame.stack.pushLong(c & 0xffffl)
            F.pure(Left(frame))
          }
        case OpCode.I64Load16S =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readShort(ea)
            frame.stack.pushLong(c)
            F.pure(Left(frame))
          }
        case OpCode.I64Load32U =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readInt(ea)
            frame.stack.pushLong(c & 0xffffffffl)
            F.pure(Left(frame))
          }
        case OpCode.I64Load32S =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readInt(ea)
            frame.stack.pushLong(c)
            F.pure(Left(frame))
          }
        case OpCode.F32Load =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readFloat(ea)
            frame.stack.pushFloat(c)
            F.pure(Left(frame))
          }
        case OpCode.F64Load =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c = mem.readDouble(ea)
            frame.stack.pushDouble(c)
            F.pure(Left(frame))
          }
        case OpCode.I32Store =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            mem.writeInt(ea, c)
            F.pure(Left(frame))
          }
        case OpCode.I32Store8 =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 8)).toByte
            mem.writeByte(ea, c1)
            F.pure(Left(frame))
          }
        case OpCode.I32Store16 =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popInt()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1 << 16)).toShort
            mem.writeShort(ea, c1)
            F.pure(Left(frame))
          }
        case OpCode.I64Store =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            mem.writeLong(ea, c)
            F.pure(Left(frame))
          }
        case OpCode.I64Store8 =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 1 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 8)).toByte
            mem.writeByte(ea, c1)
            F.pure(Left(frame))
          }
        case OpCode.I64Store16 =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 2 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 16)).toShort
            mem.writeShort(ea, c1)
            F.pure(Left(frame))
          }
        case OpCode.I64Store32 =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popLong()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            val c1 = (c % (1l << 32)).toInt
            mem.writeInt(ea, c1)
            F.pure(Left(frame))
          }
        case OpCode.F32Store =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popFloat()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 4 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            mem.writeFloat(ea, c)
            F.pure(Left(frame))
          }
        case OpCode.F64Store =>
          val offset = frame.readInt()
          val align = frame.readInt()
          val mem = frame.instance.memory(0)
          val c = frame.stack.popDouble()
          val i = frame.stack.popInt()
          val ea = i + offset
          if (offset < 0 || ea < 0 || ea + 8 > mem.size) {
            F.raiseError(new InterpreterException(frame, "out of bounds memory access"))
          } else {
            mem.writeDouble(ea, c)
            F.pure(Left(frame))
          }
        case OpCode.MemorySize =>
          val mem = frame.instance.memory(0)
          val sz = mem.size / pageSize
          frame.stack.pushInt(sz)
          F.pure(Left(frame))
        case OpCode.MemoryGrow =>
          val mem = frame.instance.memory(0)
          val sz = mem.size / pageSize
          val n = frame.stack.popInt()
          if (mem.grow(n)) {
            frame.stack.pushInt(sz)
          } else {
            frame.stack.pushInt(-1)
          }
          F.pure(Left(frame))
        // === control instructions ===
        case OpCode.Nop =>
          F.pure(Left(frame))
        case OpCode.Unreachable =>
          F.raiseError(new InterpreterException(frame, "unreachable executed"))
        case OpCode.Block =>
          // next int is the block arity
          val arity = frame.readInt()
          // next int is the block size
          val blockSize = frame.readInt()
          val lbl = Label(arity, frame.pc + blockSize + 1 /* end */ )
          frame.stack.pushLabel(lbl)
          // frame.pc is now on the first instruction of the block
          F.pure(Left(frame))
        case OpCode.End =>
          // pop the m values on top of the stack
          val values = frame.stack.popValues()
          val lbl = frame.stack.popLabel()
          // push values back
          frame.stack.pushValues(values)
          // frame.pc is now after the end of the block
          F.pure(Left(frame))
        case OpCode.Loop =>
          // continuation will be the frame.pc of the loop opcode
          val cont = frame.pc - 1
          // skip the block arity
          frame.readInt()
          // push loop label
          frame.stack.pushLabel(Label(0, cont))
          // frame.pc is now on the first instruction of the loop
          F.pure(Left(frame))
        case OpCode.If =>
          // next int is the block arity
          val arity = frame.readInt()
          // next int gives the size of the then branch in bytes
          val thenSize = frame.readInt()
          // next int gives the size of the else branch in bytes
          val elseSize = frame.readInt()
          val c = frame.stack.popBool()
          val lbl = Label(arity, frame.pc + thenSize + 1 /* else */ + 4 /* else size */ + elseSize + 1 /* end */ )
          frame.stack.pushLabel(lbl)
          // frame.pc is now on the first instruction of the then branch
          if (!c) {
            // move frame.pc to the first instruction of the else branch
            frame.pc += thenSize + 1 + 4
          }
          F.pure(Left(frame))
        case OpCode.Else =>
          // next int is the size of the else branch
          val elseSize = frame.readInt()
          // pop the m values on top of the stack
          val values = frame.stack.popValues()
          frame.stack.popLabel()
          // push values back
          frame.stack.pushValues(values)
          // put frame.pc after the end of the else block
          frame.pc += elseSize + 1 /* end */
          F.pure(Left(frame))
        case OpCode.Br =>
          // next integer is the label
          val l = frame.readInt()
          br(frame, l)
          F.pure(Left(frame))
        case OpCode.BrIf =>
          // next integer is the label
          val l = frame.readInt()
          val c = frame.stack.popBool()
          if (c)
            br(frame, l)
          F.pure(Left(frame))
        case OpCode.BrTable =>
          // next int gives the number of labels to come
          val nl = frame.readInt()
          val lbls = for (_ <- 0 until nl) yield frame.readInt()
          // next comes the default label
          val ln = frame.readInt()
          // get the label index from stack
          val i = frame.stack.popInt()
          val l = if (i >= 0 && i < lbls.size) lbls(i) else ln
          br(frame, l)
          F.pure(Left(frame))
        case OpCode.Return =>
          val values = frame.stack.popValues(frame.arity)
          // pop the frame to get the parent
          val frame1 = frame.stack.popFrame()
          if (frame1.isToplevel) {
            // this is the top-level call, return, as we are done
            F.pure(Right(values.headOption))
          } else {
            // push values back to the frame
            frame1.stack.pushValues(values)
            // continue where we left the frame
            F.pure(Left(frame1))
          }
        case OpCode.Call =>
          // next integer is the function index
          val fidx = frame.readInt()
          val f = frame.instance.function(fidx)
          invoke(frame, f)
        case OpCode.CallIndirect =>
          // next integer is the typ index
          val tidx = frame.readInt()
          val tab = frame.instance.table(0)
          val expectedt = frame.instance.tpe(tidx)
          val i = frame.stack.popInt()
          if (i >= tab.size || tab(i) == null) {
            F.raiseError(new InterpreterException(frame, "undefined element"))
          } else {
            val f = tab(i)
            val actualt = f.tpe
            if (expectedt != actualt) {
              F.raiseError(new InterpreterException(frame, "indirect call type mismatch"))
            } else {
              invoke(frame, f)
            }
          }
        case opcode =>
          F.raiseError(new InterpreterException(frame, "unknown opcode"))
      }
    }

  private def br(frame: Frame[F], l: Int): Unit = {
    // get the l-th label
    val lbl = frame.stack.getLabel(l)
    val arity = lbl.arity
    val cont = lbl.cont
    // pop the n values
    val values = frame.stack.popValues(arity)
    // pop all intermediate labels and values
    for (_ <- 0 to l) {
      frame.stack.popValues()
      frame.stack.popLabel()
    }
    // push back return values
    frame.stack.pushValues(values)
    // jump to continuation
    frame.pc = cont
  }

  private def invoke(frame: Frame[F], f: Function[F]): F[Either[Frame[F], Option[Value]]] =
    f match {
      case FunctionInstance(tpe, locals, code, inst) =>
        val ilocals = Array.ofDim[Value](locals.size + tpe.params.size)
        val zlocals = locals.map(Value.zero(_)).toArray
        Array.copy(zlocals, 0, ilocals, tpe.params.size, zlocals.length)
        // pop the parameters from the stack
        val params = frame.stack.popValues(tpe.params.size).toArray
        Array.copy(params, 0, ilocals, 0, params.length)
        val frame1 = frame.stack.pushFrame(tpe.t.size, code, ilocals, inst)
        // push the implicit block label on the called frame
        frame1.stack.pushLabel(Label(tpe.t.size, code.limit - 1))
        F.pure(Left(frame1))
      case f =>
        // pop the parameters from the stack
        val params = frame.stack.popValues(f.tpe.params.size)
        // invoke the host function with the parameters
        f.invoke(params.toVector).map { res =>
          if(frame.isToplevel) {
            Right(res)
          } else {
            res.foreach(frame.stack.pushValue(_))
            Left(frame)
          }
        }
    }

}

class InterpreterException[F[_]](frame: Frame[F], msg: String) extends RuntimeException(msg)
