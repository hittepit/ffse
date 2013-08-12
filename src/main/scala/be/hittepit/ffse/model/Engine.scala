package be.hittepit.ffse.model

object StateType extends Enumeration{
  type StateType = Value
  val START, END, STATE = Value
}

import StateType._

case class Event(name:String)

case class Engine(val name:String, val version:String, val events:List[Event],val commands:List[Command],val startState:State,val states:List[State]){
  class EngineExecutorBuilder(val fromStateName:String){
    val fromState = (startState::states).find(s => s.name==fromStateName) match{
      case Some(s) => s
      case None => throw new Exception(fromStateName+" not found in this engine")
    }
    var eventName:Option[String] = None
    
    def when(eventName:String) = {
      this.eventName = Some(eventName)
      this
    }
    
    def prospect = this.eventName match{
      case None => throw new Exception("No event defined. Use when.")
      case Some(e) =>
      fromState.transitions.find(t => t.eventName == e) match {
        case Some(t) => t.stateName
        case None => throw new Exception("event "+e+" not found on state "+fromStateName)
      }
    }
  }
  
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
  
  val engineId=name+"::"+version
  
  def hasErrors = ! errs.isEmpty
  def errors:List[_ <: EngineValidationError] = errs
  
  def from(fromStateName:String) = new EngineExecutorBuilder(fromStateName)
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
