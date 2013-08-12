package be.hittepit.ffse.model

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import be.hittepit.ffse.util.TestUtil

class TestEngineValidation extends FunSuite with MustMatchers with TestUtil{
	test("engine initialization fails if a state's transition uses an unknown event"){
		val events = List(Event("doit"))
		val startState = State("start",Nil,List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("unknown","end")),StateType.STATE)
		val engine = Engine("test","1.0",events,Nil,startState,List(endState,state))
		engine.initialize must be(false)
		engine.errors must have size(1)
		val e = engine.errors(0)
		e must be (anInstanceOf[UndefinedEventError])
		val ex = e.asInstanceOf[UndefinedEventError]
		ex.eventName must be("unknown")
		ex.stateName must be("next")
	}
	test("engine initialization fails if a state's transition uses an unknown state"){
		val events = List(Event("doit"),Event("go"))
		val startState = State("start",Nil,List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","unknown")),StateType.STATE)
		val engine = Engine("test","1.0",events,Nil,startState,List(endState,state))
		engine.initialize must be(false)
		engine.errors must have size(1)
		val e = engine.errors(0)
		e must be (anInstanceOf[UndefinedStateError])
		val ex = e.asInstanceOf[UndefinedStateError]
		ex.stateName must be("next")
		ex.destinationName must be("unknown")
		ex.eventName must be("go")
	}
	test("engine initialization fails if a state's action uses an unknown command"){
	  
	}
}