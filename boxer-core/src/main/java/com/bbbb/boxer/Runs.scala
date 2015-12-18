package com.bbbb.boxer

import com.bbbb.boxer.BoxerAction.shared
import FileOps._
import scala.sys.process._

sealed trait Runner {
  def stop()
}

object Runner {
  def apply(member: WebMember, writer: Writer) = member match {
    case WebMember(dir, memberType, memberRun, Some(memberStop), order) =>
      CmdRunner(dir, memberRun, memberStop, writer)

    case WebMember(dir, memberType, memberRun, None, order) =>
      ProcRunner(Process(member.memberRun, path(dir)).run())
  }
}

case class CmdRunner(dir: String, upCmd: String, downCmd: String, writer: Writer) extends Runner {
  writer.write(Process(upCmd, path(dir)).!!)

  override def stop(): Unit = writer.write(downCmd.!!)
}

case class ProcRunner(proc: Process) extends Runner {
  override def stop(): Unit = proc.destroy()
}


case class RunStart(name: String) extends BoxerAction {
  override def run(global: BoxerGlobal, writer: Writer): Option[Rejection] = {
    val data = shared.readData(global)

    data.webs.get(name) match {
      case Some(web) =>
        val orderedMembers = web.members.values.toList.sortBy(_.order)

        val processes = orderedMembers.map { member =>
          writer.write(s"Starting ${member.dir}")
          Runner(member, writer)
        }

        writer.write("Press any key to halt running processes")
        System.in.read()
        processes.foreach(_.stop())

        None
      case None => Some(Rejection(2, s"Web '$name' does not exist"))
    }
  }
}
