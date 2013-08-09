package be.hittepit.ffse.parser

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import be.hittepit.ffse.model.Machine
import be.hittepit.ffse.model.Command

class TestFfseParser extends FunSuite with MustMatchers{
	test("command must be parsed"){
	  val text = "justDoIt => be.hittepit.DoIt"
	  val r = FfseParser.parseAll(FfseParser.command, text)
	  r.successful must be (true)
	  r.get must be (Command("justDoIt","be.hittepit.DoIt"))
	}
  
	test("single machine with correct name must be created"){
	  val text = """machine test end"""
	  val m = FfseParser.parseAll(FfseParser.machines,text)
	  m.isEmpty must be (false)
	  m.get must have size(1)
	  val machine = m.get(0)
	  machine.name must be("test")
	}
	
	test("list of machines with correct name must be created"){
	  val text = """machine test1 end
	    machine    test2  end"""
	  val m = FfseParser.parseAll(FfseParser.machines,text)
	  m.isEmpty must be (false)
	  m.get must have size(2)
	  m.get must contain (Machine("test1"))  
	  m.get must contain (Machine("test2"))  
	}
}