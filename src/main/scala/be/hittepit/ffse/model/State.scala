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

class EngineValidationError

case class UndefinedEventError(val eventName:String, val stateName:String) extends EngineValidationError{
  override def toString = "State "+stateName+" uses an event "+eventName+" which was not defined as an event"
}

case class UndefinedStateError(val eventName:String, val destinationName:String, val stateName:String) extends EngineValidationError{
  override def toString = "State "+stateName+" defines an event "+eventName+" that leads to an unknown state "+destinationName
}

case class UndefinedActionError(val actionName:String,val stateName:String) extends EngineValidationError{
  override def toString = "State "+stateName+" uses action "+actionName+" which is not defined"
}

case class ActionClassNotFoundError(val actionName:String, val className:String) extends EngineValidationError{
  override def toString = "Class "+className+" not found for action "+actionName
}

case class ActionClassWrongTypeError(val actionName:String, val className:String) extends EngineValidationError{
  override def toString = "Class "+className+" for action "+actionName+" does not implement Executor"
}

case class Event(name:String)

case class Engine(val name:String, val version:String, val events:List[Event],commands:List[Command],startState:State,states:List[State]){
  private var errs:List[EngineValidationError] = List()
  
  def initialize:Boolean = {
	def check(state:State){
	  state.transitions.foreach{t =>
	    if(! events.exists(e=> e.name == t.eventName)){
	      errs = new UndefinedEventError(t.eventName,state.name)::errs
	    }
	    if(! states.exists(s => s.name == t.stateName) && startState.name != t.stateName){
	      errs = new UndefinedStateError(t.eventName,t.stateName,state.name)::errs
	    }  
	  }
	  state.actionsName.foreach{actionName =>
	    if(! commands.exists(c => c.name == actionName)){
	      errs = new UndefinedActionError(actionName, state.name)::errs
	    }
	  }
	}
    
    check(startState)
    states.foreach(check(_))
    commands.foreach((c) => c.error match{
      case Some(e) => errs = e::errs
      case _ =>
    })
    
    errs.isEmpty
  }
  
  def errors:List[_ <: EngineValidationError] = errs
}

case class Command(name:String,className:String){
   private var err:Option[EngineValidationError] = None
  
  val actionClass:Class[Executor] = try {
    val c = Class.forName(className)
    val mc = Manifest.classType(c)
    val baseM = Manifest.classType(classOf[Executor])
    if(mc <:< baseM){
      c.asInstanceOf[Class[Executor]]
    } else {
      err = Some(new ActionClassWrongTypeError(name,className))
      null
    }
  } catch {
    case e:ClassNotFoundException => err = Some(new ActionClassNotFoundError(name,className))
    					null
  } 
  
  def error = err
}

case class Transition(eventName:String,stateName:String)

case class State(name:String, actionsName:List[String],transitions:List[Transition],stateType:StateType)

trait Executor{
  def execute:Unit
}
