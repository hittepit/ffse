package be.hittepit.ffse.model

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
