package be.hittepit.ffse.model

class State(val name:String, val events:Map[Event,State]) {
	def addEvent(e:Event,s:State) = events.get(e) match {
	  case None => new State(name,events+(e->s))
	  case _:Some[_] => throw new Exception("state alreadu defined")
	}
}

case class Event(name:String)

case class Machine(name:String)

case class Command(name:String,className:String)

trait Executor{
  def execute(context:Context)
}

class Context(val startState:State,val states:Map[String,State]){
}