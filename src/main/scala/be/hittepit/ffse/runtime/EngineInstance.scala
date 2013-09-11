package be.hittepit.ffse.runtime

import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Executor

class EngineInstance(engine:Engine) {
	val engineId=engine.engineId
	
	def findNextState(stateName:String, event:String):String = engine.from(stateName).when(event)
	
	def execute(stateName:String, event:String, context:Map[String,Any]):String = engine.from(stateName).execute(event,context)
}