package be.hittepit.ffse.model

object StateType extends Enumeration{
  type StateType = Value
  val START, END, STATE = Value
}

import StateType._

case class Event(name:String)

case class Engine(val name:String, val version:String, val events:List[Event],val commands:List[Command],val startState:State,val states:List[State]){
  class EngineExecutorBuilder(val fromStateName:String){
    val fromState = findState(fromStateName)

    var eventName:Option[String] = None
    
    def when(eventName:String) = {
      fromState.transitions.find(t => t.eventName == eventName) match {
        case Some(t) => t.stateName
        case None => throw new Exception("event "+eventName+" not found on state "+fromStateName)
      }
    }
    
    def execute(eventName:String,context:Map[String,Any]) = {
      fromState.transitions.find(t => t.eventName == eventName) match {
        case Some(t) => val nextState = findState(t.stateName)
        				val commandClasses = nextState.actionsName.map(findCommand(_))
        				commandClasses.foreach{ command =>
        					val c = Class.forName(command.className)
        					val commandInstance = c.newInstance().asInstanceOf[Executor]
        					commandInstance.execute(context)
        				}
          				nextState.name
        case None => throw new Exception("event "+eventName+" not found on state "+fromStateName)
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
  
  private def findState(stateName:String) = (startState::states).find(s => s.name==stateName) match{
      case Some(s) => s
      case None => throw new Exception(stateName+" not found in this engine")
    }
  
  private def findCommand(commandName:String) = commands.find(c => c.name == commandName) match{
    case Some(command) => command
    case None => throw new Exception(commandName+" not found in this engine")
  }
  
  val engineId=name+"::"+version
  
  def hasErrors = ! errs.isEmpty
  def errors:List[_ <: EngineValidationError] = errs
  
  def from(fromStateName:String) = new EngineExecutorBuilder(fromStateName)
  
  def actions(stateName:String):List[Class[Executor]] = (startState::states).find(s => s.name==stateName) match{
    case None => Nil
    case Some(state) => state.actionsName.map{an => commands.find{c => c.name==an} match{
      							case None => throw new Exception("Pas possible, voir validation")
      							case Some(c) => c.actionClass
    						}
    					}
  }
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
  def execute(context:Map[String,Any]):Unit
}
