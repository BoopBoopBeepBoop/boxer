package com.bbbb.boxer

import java.nio.file.Paths

import scala.util.control.NonFatal
import FileOps._

/**
  */
object Boxer extends App {

  val global = BoxerGlobal(
    userHome = Paths.get(System.getProperty("user.home"))
  )

  val maybeRejection =
    try {
      args.toList match {
        case "create" :: name :: Nil =>
          new WebCreate(name).run(global, PrintlnWriter)

        case "add" :: name :: dir :: cmd :: Nil =>
          new WebAdd(name, "default", Paths.get(dir).toFile, cmd, None).run(global, PrintlnWriter)

        case "add" :: name :: dir :: "bg" :: cmd :: downCmd :: Nil =>
          new WebAdd(name, "bg", path(dir), cmd, Some(downCmd)).run(global, PrintlnWriter)

        case "remove" :: name :: dir :: Nil =>
          new WebRemove(name, path(dir)).run(global, PrintlnWriter)

        case "order" :: name :: path :: action :: Nil =>
          new WebReorder(name, path, action).run(global, PrintlnWriter)

        case "describe" :: name :: Nil =>
          new WebDescribe(name).run(global, PrintlnWriter)

        case "list" :: Nil =>
          new WebList().run(global, PrintlnWriter)

        case "run" :: name :: Nil =>
          new RunStart(name).run(global, PrintlnWriter)

        case "delete" :: Nil =>
          new DeleteAll().run(global, PrintlnWriter)

        case other => Some(Rejection(42, "unknown arguments matched no commands"))
      }
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        Some(Rejection(41, "unknown error"))
    }

  maybeRejection.foreach { r =>
    System.err.println(r.msg)
    sys.exit(r.code)
  }
}
