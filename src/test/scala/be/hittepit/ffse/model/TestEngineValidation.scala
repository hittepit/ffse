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
		val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","be.hittepit.ffse.model.TestExecutor"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",List("unknown"),List(Transition("go","end")),StateType.STATE)
		val engine = Engine("test","1.0",events,commands,startState,List(endState,state))
		engine.initialize must be(false)
		engine.errors must have size(1)
		
		engine.errors(0) must be(anInstanceOf[UndefinedActionError])
		
		val e = engine.errors(0).asInstanceOf[UndefinedActionError]
		e.actionName must be("unknown")
		e.stateName must be("next")
	}
	test("engine validation fails if a command creation fails"){
		val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","java.lang.String"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","end")),StateType.STATE)
		val engine = Engine("test","1.0",events,commands,startState,List(endState,state))
		engine.initialize must be(false)
		engine.errors must have size(1)
		
		engine.errors(0) must be(anInstanceOf[ActionClassWrongTypeError])
		
		val e = engine.errors(0).asInstanceOf[ActionClassWrongTypeError]
		e.actionName must be("action")
		e.className must be("java.lang.String")
	}
	test("engine validation fails and errors contains all errors found"){
		val events = List(Event("go"))
		val commands = List(Command("action1","java.lang.String"),Command("action2","java.lang.Toto"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","yoyo")),StateType.STATE)
		val engine = Engine("test","1.0",events,commands,startState,List(endState,state))
		engine.initialize must be(false)
		val errors = engine.errors
		errors must have size(5)
		
		errors must contain(UndefinedEventError("doit", "start").asInstanceOf[EngineValidationError])
		errors must contain(UndefinedStateError("go", "yoyo", "next").asInstanceOf[EngineValidationError])
		errors must contain(UndefinedActionError("action", "start").asInstanceOf[EngineValidationError])
		errors must contain(ActionClassNotFoundError("action2", "java.lang.Toto").asInstanceOf[EngineValidationError])
		errors must contain(ActionClassWrongTypeError("action1", "java.lang.String").asInstanceOf[EngineValidationError])
	}
}
