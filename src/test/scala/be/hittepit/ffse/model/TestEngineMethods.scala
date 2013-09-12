package be.hittepit.ffse.model

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers

object MockInvocation{
  var invocations = 0
}

class MockTestExecutor extends Executor{
  def execute(context:Map[String,Any]){
    context.get("invocation") match {
      case Some(i:Int) => MockInvocation.invocations +=i
      case None => MockInvocation.invocations +=1
    }
  }
}

class TestEngineMethods extends FunSuite with ShouldMatchers with BeforeAndAfter{
	val engine = {
	  	val events = List(Event("doit"),Event("go"))
		val commands = List(Command("action","be.hittepit.ffse.model.MockTestExecutor"))
		val startState = State("start",Nil,List(Transition("doit","next")),StateType.START)
		val endState = State("end",Nil,Nil,StateType.END)
		val state = State("next",List("action"),List(Transition("go","end")),StateType.STATE)
		Engine("test",None,"1.0",events,commands,startState,List(endState,state))
	}
	
	before{
		MockInvocation.invocations = 0
	}
  
	test("from when returns the target state without executing any action"){
	  	var result = engine.from("start").when("doit")
	  	result should be("next")
	  	MockInvocation.invocations should be(0)
	  	
	  	result = engine.from("next").when("go")
	  	result should be("end")
	  	MockInvocation.invocations should be(0)
	}
	
	test("from when throws an exception if from state does not exist and does not execute any action"){
		evaluating(engine.from("notdefined").when("go")) should produce[Exception]
	}
	
	test("from when throws an exception if event is not handle be from state and does not execute any action"){
		evaluating(engine.from("next").when("gone")) should produce[Exception]
	}
	
	test("from execute returns the target after having executed the actions"){
	  	var result = engine.from("start").execute("doit",Map())
	  	result should be("next")
	  	MockInvocation.invocations should be(1)
	  	
	  	result = engine.from("next").execute("go",Map())
	  	result should be("end")
	  	MockInvocation.invocations should be(1)
	}
	
	test("context should be passed to executor"){
	  	var result = engine.from("start").execute("doit",Map("invocation"->100))
	  	result should be("next")
	  	MockInvocation.invocations should be(100)
	}
	
	test("from execute throws an exception if from state does not exist and does not execute any action"){
		evaluating(engine.from("notdefined").execute("go",Map())) should produce[Exception]
	}
	
	test("from execute throws an exception if event is not handle be from state and does not execute any action"){
		evaluating(engine.from("next").execute("gone",Map())) should produce[Exception]
	}
}