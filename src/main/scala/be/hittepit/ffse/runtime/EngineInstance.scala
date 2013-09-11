package be.hittepit.ffse.runtime

import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Executor

class EngineInstance(val instanceId:String, engine:Engine) {
	val engineId=engine.engineId
	
	var currentState:String = engine.start

	//TODO explore next state, returns everything
	//TODO change the actions for a state?
	
	def findNextState(stateName:String, event:String):String = engine.from(stateName).when(event)
	
	def execute(event:String, context:Map[String,Any]):String = {
	  val newState = engine.from(currentState).execute(event,context)
	  currentState = newState
	  currentState
	}
}