package be.hittepit.ffse.model

import org.scalatest.matchers.MustMatchers
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

object MockInvocation{
  var invocations = 0
}

class MockTestExecutor extends Executor{
  def execute{
    MockInvocation.invocations +=1
  }
}

class TestEngineMethods extends MustMatchers with FunSuite with BeforeAndAfter{
	val engine = {
	  	val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","be.hittepit.ffse.model.MockTestExecutor"))
		val startState = State("start",List("action"),List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",Nil,List(Transition("go","end")),StateType.STATE)
		Engine("test","1.0",events,commands,startState,List(endState,state))
	}
	
	before{
		MockInvocation.invocations = 0
	}
  
	test("from when prospect returns the target state without executing any action"){
	  	var result = engine.from("start").when("doit")
	  	result must be("next")
	  	MockInvocation.invocations must be(0)
	  	
	  	result = engine.from("next").when("go")
	  	result must be("end")
	  	MockInvocation.invocations must be(0)
	}
	
	test("from when prospect throws an exception if from state does not exist and does not execute any action"){
		evaluating(engine.from("notdefined").when("go")) must produce[Exception]
	}
	
	test("from when prospect throws an exception if event is not handle be from state and does not execute any action"){
		evaluating(engine.from("next").when("gone")) must produce[Exception]
	}
}