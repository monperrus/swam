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

import cats.effect._

import java.nio.file.Path
import java.util.concurrent.Executors

import scala.io.Source
import scala.concurrent.ExecutionContext

import scala.language.higherKinds

import fs2.{io, text => stext}

package object text {

  val blockingExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))

  def readFile(f: Path)(implicit cs: ContextShift[IO]): IO[String] =
    io.file
      .readAll[IO](f, blockingExecutionContext, 4096)
      .through(stext.utf8Decode)
      .compile
      .fold(new StringBuilder) { (sb, s) =>
        sb.append(s)
      }
      .map(_.result)

}
