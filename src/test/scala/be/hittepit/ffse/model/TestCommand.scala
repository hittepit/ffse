package be.hittepit.ffse.model

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import be.hittepit.ffse.util.TestUtil

class TestExecutor extends Executor{
  def execute(context:Map[String,Any]) = println("ok")
}

class TestCommand extends FunSuite with MustMatchers with TestUtil{
	
	test("class must be found if it exists"){
	  val c = Command("test","be.hittepit.ffse.model.TestExecutor")
	  c.error must be(None)
	  c.actionClass must be((new TestExecutor()).getClass())
	}
	
	test("if class cannot be found, error contains a ActionClassNotFoundError"){
	  val c = Command("test","be.NeverExist")
	  c.error must be('defined)
	  c.error.get must be(anInstanceOf[ActionClassNotFoundError])
	  val e = c.error.get.asInstanceOf[ActionClassNotFoundError]
	  e.actionName must be("test")
	  e.className must be("be.NeverExist")
	}
	
	test("if class is not Executor, error contains a ActionClassWrongTypeError"){
	  val c = Command("test","be.hittepit.ffse.model.TestCommand")
	  c.error must be('defined)
	  c.error.get must be(anInstanceOf[ActionClassWrongTypeError])
	  val e = c.error.get.asInstanceOf[ActionClassWrongTypeError]
	  e.actionName must be("test")
	  e.className must be("be.hittepit.ffse.model.TestCommand")
	}
}