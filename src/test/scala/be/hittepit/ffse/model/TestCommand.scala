package be.hittepit.ffse.model

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import be.hittepit.ffse.util.TestUtil

class TestExecutor extends Executor{
  def execute = println("ok")
}

class TestCommand extends FunSuite with MustMatchers with TestUtil{
	
	test("class must be found if it exists"){
	  val c = Command("test","be.hittepit.ffse.model.TestExecutor")
	  c.error must be(None)
	  c.actionClass must be((new TestExecutor()).getClass())
	}
	
	test("if class cannot be found, error contains a ClassNotFoundException"){
	  val c = Command("test","be.NeverExist")
	  c.error must be('defined)
	  c.error.get must be(anInstanceOf[ClassNotFoundException])
	}
}