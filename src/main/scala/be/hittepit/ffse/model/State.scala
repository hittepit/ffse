package be.hittepit.ffse.model

//class State(val name:String, val events:Map[Event,State]) {
//	def addEvent(e:Event,s:State) = events.get(e) match {
//	  case None => new State(name,events+(e->s))
//	  case _:Some[_] => throw new Exception("state alreadu defined")
//	}
//}

object StateType extends Enumeration{
  type StateType = Value
  val START, END, STATE = Value
}

import StateType._

case class Event(name:String)

case class Engine(val name:String, val version:String, val events:List[Event],commands:List[Command],startState:State,states:List[State]){
  def initialize{
    
  }
  
  def errors:List[Exception] = Nil
}

case class Command(name:String,className:String){
   private var err:Option[Exception] = None
  
  val actionClass:Class[Executor] = try {
    val c = Class.forName(className)
    val mc = Manifest.classType(c)
    val baseM = Manifest.classType(classOf[Executor])
    if(mc <:< baseM){
      c.asInstanceOf[Class[Executor]]
    } else {
      err = Some(new Exception(className+" n'est pas une action"))
      null
    }
  } catch {
    case e:Exception => err = Some(e)
    					null
  } 
  
  def error = err
}

case class Transition(eventName:String,stateName:String)

case class State(name:String, actionsName:List[String],transitions:List[Transition],stateType:StateType)

trait Executor{
  def execute:Unit
}
