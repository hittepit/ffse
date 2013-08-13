package be.hittepit.ffse.runtime

import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Executor

class EngineRuntime(engine:Engine) {
	val engineId=engine.engineId
	
	def findNextState(stateName:String, event:String):String = throw new Exception("not implemented yet")
	
	def execute(stateName:String, event:String):Boolean = throw new Exception("not implemented yet")
}