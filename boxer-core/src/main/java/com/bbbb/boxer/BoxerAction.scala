package com.bbbb.boxer

import java.io.File
import java.nio.file.{Files, Path}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import BoxerAction._
import FileOps._

import scala.util.Try

case class BoxerGlobal(userHome: Path)

trait BoxerAction {
  def run(boxerGlobal: BoxerGlobal, writer: Writer): Option[Rejection]
}

trait Writer {
  def write(s: String): Unit
}

object PrintlnWriter extends Writer {
  override def write(s: String): Unit = println(s)
}

object BoxerAction {
  object shared {
    val mapper = {
      val m = new ObjectMapper()
      m.registerModule(DefaultScalaModule)
      m
    }

    def dataFile(global: BoxerGlobal) = global.userHome / ".boxer" / "data"

    def runFile(global: BoxerGlobal) = global.userHome / ".boxer" / "run"

    // ensure root file exists
    def ensureRoot(global: BoxerGlobal) = {
      if (!dataFile(global).exists()) {
        Files.createDirectories(dataFile(global).getParent)
        writeData(global)(BoxerData(Map.empty))
      }
    }

    def writeData(global: BoxerGlobal)(data: BoxerData): Unit = {
      mapper.writeValue(dataFile(global), data)
    }

    def readData(global: BoxerGlobal): BoxerData = {
      ensureRoot(global)
      mapper.readValue(dataFile(global), classOf[BoxerData])
    }

    def readRun(global: BoxerGlobal): RunData = {
      mapper.readValue(runFile(global), classOf[RunData])
    }
  }
}

case class Rejection(code: Int, msg: String)

class DeleteAll() extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    shared.dataFile(global).fDelete
    None
  }
}

class WebCreate(name: String) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val data = shared.readData(global)

    if(data.webs.contains(name)) {
      Some(Rejection(1, s"Name '$name' already exists"))
    } else {
      val newData = data.copy(webs = data.webs + (name -> Web(name, Map.empty)))
      shared.writeData(global)(newData)
      None
    }
  }
}

case class WebReorder(name: String, target: String, action: String) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val data = shared.readData(global)

    val dirString = path(target).toAbsolutePath.normalize().toString

    data.webs.get(name) match {
      case Some(web) =>
        val keys = web.members.keys.toSet
        if(!keys.contains(dirString)) {
          Some(Rejection(2, s"Path '$target' not contained with web '$name'"))
        } else {

          val m = web.members(dirString)
          val newMember = action match {
            case "--" => m.copy(order = m.order - 1)
            case "++" => m.copy(order = m.order + 1)
          }

          val newMembers = web.members + (dirString -> newMember)
          shared.writeData(global)(
            data.copy(webs = data.webs + (web.name -> web.copy(members = newMembers)))
          )
          None
        }

      case None => Some(Rejection(2, s"Web '$name' does not exist"))
    }
  }
}

case class WebAdd(name: String, thingType: String, dir: File, runcmd: String, downcmd: Option[String]) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val data = shared.readData(global)

    data.webs.get(name) match {
      case Some(web) =>

        val dirString = dir.toPath.toAbsolutePath.normalize().toString
        val newMember = WebMember(
          dir = dirString,
          memberType = thingType,
          memberRun = runcmd,
          memberStop = downcmd
        )

        val newMembers = web.members + (dirString -> newMember)
        shared.writeData(global)(
          data.copy(webs = data.webs + (web.name -> web.copy(members = newMembers)))
        )
        None

      case None => Some(Rejection(2, s"Web '$name' does not exist"))
    }
  }
}

case class WebList() extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    shared.readData(global).webs.keys.foreach(writer.write)
    None
  }
}

case class WebDescribe(name: String) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val foo = shared.readData(global).webs.get(name).map { web =>
      writer.write(s"Web [name=${web.name}]")
      TableBuilder(web.members.values).foreach { row =>
        writer.write(row.mkString(""))
      }

      None
    }

    foo.getOrElse(Some(Rejection(2, s"Web '$name' does not exist")))
  }
}



case class WebStatus(name: String) extends BoxerAction {
  override def run(boxerGlobal: BoxerGlobal, writer: Writer): Option[Rejection] = ???
}

case class WebRemove(name: String, dir: File) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val data = shared.readData(global)

    data.webs.get(name) match {
      case Some(web) =>
        val dirString = dir.toPath.toAbsolutePath.normalize().toString

        if (!web.members.contains(dirString)) {
          Some(Rejection(2, s"Path '$dirString' not contained with web '$name'"))
        } else {
          val newMembers = web.members - dirString
          shared.writeData(global)(
            data.copy(webs = data.webs + (web.name -> web.copy(members = newMembers)))
          )
          None
        }

      case None => Some(Rejection(2, s"Web '$name' does not exist"))
    }
  }
}