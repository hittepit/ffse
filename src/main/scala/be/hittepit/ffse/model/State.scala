package be.hittepit.ffse.model

object StateType extends Enumeration{
  type StateType = Value
  val START, END, STATE = Value
}

import StateType._

class EngineValidationError

case class UndefinedEventError(val engine:String, val eventName:String, val stateName:String) extends EngineValidationError{
  override def toString = "Engine "+engine+": state "+stateName+" uses an event "+eventName+" which was not defined as an event"
}

case class UndefinedStateError(val engine:String, val eventName:String, val destinationName:String, val stateName:String) extends EngineValidationError{
  override def toString = "Engine "+engine+": state "+stateName+" defines an event "+eventName+" that leads to an unknown state "+destinationName
}

case class UndefinedActionError(val engine:String, val actionName:String,val stateName:String) extends EngineValidationError{
  override def toString = "Engine "+engine+": state "+stateName+" uses action "+actionName+" which is not defined"
}

case class ActionClassNotFoundError(val actionName:String, val className:String) extends EngineValidationError

case class ActionClassNotFoundErrorInCommand(val engine:String, val actionName:String, val className:String) extends EngineValidationError{
  override def toString = "Engine "+engine+": class "+className+" not found for action "+actionName
}

case class ActionClassWrongTypeError(val actionName:String, val className:String) extends EngineValidationError

case class ActionClassWrongTypeErrorInCommand(val engine:String, val actionName:String, val className:String) extends EngineValidationError{
  override def toString = "Engine "+engine+": class "+className+" for action "+actionName+" does not implement Executor"
}

case class Event(name:String)

case class Engine(val name:String, val version:String, val events:List[Event],commands:List[Command],startState:State,states:List[State]){
  private var errs:List[EngineValidationError] = List()
  
  initialize
  
  private def initialize {
	def check(state:State){
	  state.transitions.foreach{t =>
	    if(! events.exists(e=> e.name == t.eventName)){
	      errs = new UndefinedEventError(name,t.eventName,state.name)::errs
	    }
	    if(! states.exists(s => s.name == t.stateName) && startState.name != t.stateName){
	      errs = new UndefinedStateError(name,t.eventName,t.stateName,state.name)::errs
	    }  
	  }
	  state.actionsName.foreach{actionName =>
	    if(! commands.exists(c => c.name == actionName)){
	      errs = new UndefinedActionError(name,actionName, state.name)::errs
	    }
	  }
	}
    
    check(startState)
    states.foreach(check(_))
    commands.foreach((c) => c.error match{
      case Some(ActionClassNotFoundError(a,c)) => errs = ActionClassNotFoundErrorInCommand(name,a,c)::errs
      case Some(ActionClassWrongTypeError(a,c)) => errs = ActionClassWrongTypeErrorInCommand(name,a,c)::errs
      case _ =>
    })
  }
  
  def hasErrors = ! errs.isEmpty
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
      err = Some(ActionClassWrongTypeError(name,className))
      null
    }
  } catch {
    case e:ClassNotFoundException => err = Some(ActionClassNotFoundError(name,className))
    										null
  } 
  
  def error = err
}

case class Transition(eventName:String,stateName:String)

case class State(name:String, actionsName:List[String],transitions:List[Transition],stateType:StateType)

trait Executor{
  def execute:Unit
}
