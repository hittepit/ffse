package be.hittepit.ffse.model

import org.scalatest.FunSuite
import be.hittepit.ffse.util.TestUtil
import org.scalatest.matchers.ShouldMatchers

class TestExecutor extends Executor{
  def execute(context:Map[String,Any]) = println("ok")
}

class TestCommand extends FunSuite with ShouldMatchers with TestUtil{
	
	test("class should be found if it exists"){
	  val c = Command("test","be.hittepit.ffse.model.TestExecutor")
	  c.error should be(None)
	  c.actionClass should be((new TestExecutor()).getClass())
	}
	
	test("if class cannot be found, error contains a ActionClassNotFoundError"){
	  val c = Command("test","be.NeverExist")
	  c.error should be('defined)
	  c.error.get should be(anInstanceOf[ActionClassNotFoundError])
	  val e = c.error.get.asInstanceOf[ActionClassNotFoundError]
	  e.actionName should be("test")
	  e.className should be("be.NeverExist")
	}
	
	test("if class is not Executor, error contains a ActionClassWrongTypeError"){
	  val c = Command("test","be.hittepit.ffse.model.TestCommand")
	  c.error should be('defined)
	  c.error.get should be(anInstanceOf[ActionClassWrongTypeError])
	  val e = c.error.get.asInstanceOf[ActionClassWrongTypeError]
	  e.actionName should be("test")
	  e.className should be("be.hittepit.ffse.model.TestCommand")
	}
}