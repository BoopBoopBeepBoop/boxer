package com.bbbb.boxer

import java.io.File
import java.nio.file.{Paths, Files, Path}
import scala.language.implicitConversions

/**
  */
object FileOps {

  def path(s: String) = Paths.get(s)

  implicit def file2Path(file: File): Path = file.toPath

  implicit def path2File(path: Path): File = path.toFile

  implicit class BetterPath(p: Path) {
    def / (s: String): Path = p.resolve(s)

    def fDelete: Unit = {
      if(Files.isDirectory(p)) p.listFiles().foreach(_.delete)
      else Files.delete(p)
    }
  }


}
