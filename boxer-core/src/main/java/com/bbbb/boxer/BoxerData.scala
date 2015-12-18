package com.bbbb.boxer


case class BoxerData(
  webs: Map[String, Web]
)

case class Web(
  name: String,
  members: Map[String, WebMember]
)

case class WebMember(
  dir: String,
  memberType: String,
  memberRun: String,
  memberStop: Option[String] = None,
  order: Int = 0
) {
  def toRunMember(pid: Int) = {
    RunMember(dir, pid, memberType, memberRun)
  }
}

case class RunMember(
  dir: String,
  pid: Int,
  memberType: String,
  memberRun: String
)

case class RunData(
  name: String,
  members: Map[String, RunMember]
)
