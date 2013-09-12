package be.hittepit.ffse.model

import org.scalatest.FunSuite
import be.hittepit.ffse.util.TestUtil
import org.scalatest.matchers.ShouldMatchers

class TestEngineValidation extends FunSuite with ShouldMatchers with TestUtil{
	test("engine initialization fails if a state's transition uses an unknown event"){
		val events = List(Event("doit"))
		val startState = State("start",Nil,List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("unknown","end")),StateType.STATE)
		val engine = Engine("test",None,"1.0",events,Nil,startState,List(endState,state))
		engine.hasErrors should be(true)
		engine.errors should have size(1)
		val e = engine.errors(0)
		e should be (anInstanceOf[UndefinedEventError])
		val ex = e.asInstanceOf[UndefinedEventError]
		ex.eventName should be("unknown")
		ex.stateName should be("next")
	}
	test("engine initialization fails if a state's transition uses an unknown state"){
		val events = List(Event("doit"),Event("go"))
		val startState = State("start",Nil,List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","unknown")),StateType.STATE)
		val engine = Engine("test",None,"1.0",events,Nil,startState,List(endState,state))
		engine.hasErrors should be(true)
		engine.errors should have size(1)
		val e = engine.errors(0)
		e should be (anInstanceOf[UndefinedStateError])
		val ex = e.asInstanceOf[UndefinedStateError]
		ex.stateName should be("next")
		ex.destinationName should be("unknown")
		ex.eventName should be("go")
	}
	test("engine initialization fails if a state's action uses an unknown command"){
		val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","be.hittepit.ffse.model.TestExecutor"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",List("unknown"),List(Transition("go","end")),StateType.STATE)
		val engine = Engine("test",None,"1.0",events,commands,startState,List(endState,state))
		engine.hasErrors should be(true)
		engine.errors should have size(1)
		
		engine.errors(0) should be(anInstanceOf[UndefinedActionError])
		
		val e = engine.errors(0).asInstanceOf[UndefinedActionError]
		e.actionName should be("unknown")
		e.stateName should be("next")
	}
	test("engine validation fails if a command creation fails"){
		val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","java.lang.String"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","end")),StateType.STATE)
		val engine = Engine("test",None,"1.0",events,commands,startState,List(endState,state))
		engine.hasErrors should be(true)
		engine.errors should have size(1)
		
		engine.errors(0) should be(anInstanceOf[ActionClassWrongTypeErrorInCommand])
		
		val e = engine.errors(0).asInstanceOf[ActionClassWrongTypeErrorInCommand]
		e.actionName should be("action")
		e.className should be("java.lang.String")
	}
	test("engine validation fails and errors contains all errors found"){
		val events = List(Event("go"))
		val commands = List(Command("action1","java.lang.String"),Command("action2","java.lang.Toto"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","yoyo")),StateType.STATE)
		val engine = Engine("test",None,"1.0",events,commands,startState,List(endState,state))
		engine.hasErrors should be(true)
		val errors = engine.errors
		errors should have size(5)
		
		errors should contain(UndefinedEventError("test","doit", "start").asInstanceOf[EngineValidationError])
		errors should contain(UndefinedStateError("test","go", "yoyo", "next").asInstanceOf[EngineValidationError])
		errors should contain(UndefinedActionError("test","action", "start").asInstanceOf[EngineValidationError])
		errors should contain(ActionClassNotFoundErrorInCommand("test","action2", "java.lang.Toto").asInstanceOf[EngineValidationError])
		errors should contain(ActionClassWrongTypeErrorInCommand("test","action1", "java.lang.String").asInstanceOf[EngineValidationError])
	}
}
