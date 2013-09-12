package be.hittepit.ffse.parser

import org.scalatest.FunSuite
import be.hittepit.ffse.model.Engine
import be.hittepit.ffse.model.Command
import be.hittepit.ffse.model.Event
import be.hittepit.ffse.model.Transition
import be.hittepit.ffse.model.StateType
import be.hittepit.ffse.model.State
import org.scalatest.matchers.ShouldMatchers

class TestFfseParser extends FunSuite with ShouldMatchers{
	test("version 1.0.0 should return 1.0.0"){
	  val text = "version 1.0.0"
	  val r = FfseParser.parseAll(FfseParser.version,text)
	  r.successful should be(true)
	  r.get should be("1.0.0")
	}
	
	test("version 2 should return 2"){
	  val text = "version 2"
	  val r = FfseParser.parseAll(FfseParser.version,text)
	  r.successful should be(true)
	  r.get should be("2")
	}
  
	test("command should be parsed"){
	  val text = "justDoIt => be.hittepit.DoIt"
	  val r = FfseParser.parseAll(FfseParser.command, text)
	  r.successful should be (true)
	  r.get should be (Command("justDoIt","be.hittepit.DoIt"))
	}
  
	test("commands should return a list of commands"){
		val text = """commands
		  test1 => be.Class1
		  test2 => be.Class2
		  test3 => be.Class3
		  end"""
		val r = FfseParser.parseAll(FfseParser.commands, text);
		r.successful should be (true)
		
		val commands = r.get
		commands should have size(3)
		commands should contain(Command("test1","be.Class1"))
		commands should contain(Command("test2","be.Class2"))
		commands should contain(Command("test3","be.Class3"))
	}
	
	test("commands with empty body returns an empty list"){
		val text = "commands end"
		val r = FfseParser.parseAll(FfseParser.commands, text);
		r.successful should be (true)
		r.get should be (List())
	}
	
	test("events should return a list of events name"){
	  val text = """events test1
	    test2
	    endEvent
	    eventendNear
	    eventEnd
	    end"""
	    
	  val r = FfseParser.parseAll(FfseParser.events,text)
	  r.successful should be(true)
	  val events = r.get
	  events should have size(5)
	  events should contain(Event("test1"))
	  events should contain(Event("test2"))
	  events should contain(Event("endEvent"))
	  events should contain(Event("eventendNear"))
	  events should contain(Event("eventEnd"))
	}
	
	test("events with empty body should return an empty list"){
	  val text = """events 
	    end"""
	    
	  val r = FfseParser.parseAll(FfseParser.events,text)
	  r.successful should be(true)
	  val events = r.get
	  events should be('empty)
	}
	
	test("actions should return a list of action names"){
	  val text = "actions{test1 test2  test3}"
	    
	  val r = FfseParser.parseAll(FfseParser.actions,text)
	  r.successful should be(true)
	  val actions = r.get
	  actions should have size(3)
	  actions should contain("test1")
	  actions should contain("test2")
	  actions should contain("test3")
	}
	
	test("actions with no action should return an empty list"){
	  val text = "actions{}"
	    
	  val r = FfseParser.parseAll(FfseParser.actions,text)
	  r.successful should be(true)
	  r.get should be('empty)
	}
	
	test("stateBody with actions should return a fully initialized StateBody object"){
	  val text = """
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	  """
	  val r = FfseParser.parseAll(FfseParser.stateBody,text)
	  r.successful should be(true)
	  val actions = r.get._1
	  val transitions = r.get._2
	  
	  actions should have size(3)
	  actions should contain("test1")
	  actions should contain("test2")
	  actions should contain("test3")
	  
	  transitions should have size(2)
	  transitions should contain(Transition("event1","state1"))
	  transitions should contain(Transition("event2","state2"))
	}
	
	test("stateBody with no actions should return a fully initialized StateBody object"){
	  val text = """
	    event1 => state1
	    event2 => state2
	  """
	  val r = FfseParser.parseAll(FfseParser.stateBody,text)
	  r.successful should be(true)
	  val actions = r.get._1
	  val transitions = r.get._2
	  
	  actions should be('empty)
	  
	  transitions should have size(2)
	  transitions should contain(Transition("event1","state1"))
	  transitions should contain(Transition("event2","state2"))
	}
	
	test("startState should create a start state fully initialized"){
	  val text = """start first
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.startState,text)
	  r.successful should be(true)
	  
	  val s = r.get
	  s.name should be("first")
	  s.stateType should be(StateType.START)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions should have size(3)
	  actions should contain("test1")
	  actions should contain("test2")
	  actions should contain("test3")
	  
	  transitions should have size(2)
	  transitions should contain(Transition("event1","state1"))
	  transitions should contain(Transition("event2","state2"))
	}
	
	test("endState should create a end state fully initialized"){
	  val text = """finish last
	    actions{ test1 test2 test3}
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.endState,text)
	  r.successful should be(true)
	  
	  val s = r.get
	  s.name should be("last")
	  s.stateType should be(StateType.END)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions should have size(3)
	  actions should contain("test1")
	  actions should contain("test2")
	  actions should contain("test3")
	  
	  transitions should be('Empty)
	}
	
	test("state should create a state fully initialized"){
	  val text = """state etat1
	    actions{ test1 test2 test3}
	    event1 => state1
	    event2 => state2
	    end
	  """
	  val r = FfseParser.parseAll(FfseParser.state,text)
	  r.successful should be(true)
	  
	  val s = r.get
	  s.name should be("etat1")
	  s.stateType should be(StateType.STATE)
	  val actions = s.actionsName
	  val transitions = s.transitions
	  actions should have size(3)
	  actions should contain("test1")
	  actions should contain("test2")
	  actions should contain("test3")
	  
	  transitions should have size(2)
	  transitions should contain(Transition("event1","state1"))
	  transitions should contain(Transition("event2","state2"))
	}
	
	test("single engine with correct name should be created"){
	  val text = """
	    engine test
			  version 1.0
	    
			  events
			  	e1 
			  	e2 
			  	e3
			  end
	    
			  commands
			  	act1 => be.test.Action1
			  	act2 => be.test.Action2
			  end
	    
			  start begin
			  	e1 => st1
			  end
	    
			  state st1
			  	actions{act1 act2}
			  	e2 => fin
			  	e3 => st2
			  end
	    
			  state st2
			  	e2 => fin
			  end

			  finish fin
			  end
	    end"""
	  val m = FfseParser.parseAll(FfseParser.engine,text)
	  m.successful should be (true)
	  val engine = m.get
	  engine.name should be("test")
	  engine.comment should be(None)
	  engine.version should be("1.0")
	  engine.commands should have size(2)
	  engine.commands should contain(Command("act1","be.test.Action1"))
	  engine.commands should contain(Command("act2","be.test.Action2"))
	  engine.events should have size(3)
	  engine.events should contain(Event("e1"))
	  engine.events should contain(Event("e2"))
	  engine.events should contain(Event("e3"))
	  engine.startState should be(State("begin",Nil,List(Transition("e1","st1")),StateType.START))
	  val states = engine.states
	  states should have size(3)
	  states should contain(State("st1",List("act1","act2"),List(Transition("e2","fin"),Transition("e3","st2")),StateType.STATE))
	  states should contain(State("st2",Nil,List(Transition("e2","fin")),StateType.STATE))
	  states should contain(State("fin",Nil,Nil,StateType.END))
	}
	
	test("single engine with correct name and comment should be created"){
	  val text = """
	    engine test
			  (Ceci est un test élémentaire)
			  version 1.0
	    
			  events
			  	e1 
			  	e2 
			  	e3
			  end
	    
			  commands
			  	act1 => be.test.Action1
			  	act2 => be.test.Action2
			  end
	    
			  start begin
			  	e1 => st1
			  end
	    
			  state st1
			  	actions{act1 act2}
			  	e2 => fin
			  	e3 => st2
			  end
	    
			  state st2
			  	e2 => fin
			  end

			  finish fin
			  end
	    end"""
	  val m = FfseParser.parseAll(FfseParser.engine,text)
	  m.successful should be (true)
	  val engine = m.get
	  engine.name should be("test")
	  engine.comment should be(Some("Ceci est un test élémentaire"))
	  engine.version should be("1.0")
	  engine.commands should have size(2)
	  engine.commands should contain(Command("act1","be.test.Action1"))
	  engine.commands should contain(Command("act2","be.test.Action2"))
	  engine.events should have size(3)
	  engine.events should contain(Event("e1"))
	  engine.events should contain(Event("e2"))
	  engine.events should contain(Event("e3"))
	  engine.startState should be(State("begin",Nil,List(Transition("e1","st1")),StateType.START))
	  val states = engine.states
	  states should have size(3)
	  states should contain(State("st1",List("act1","act2"),List(Transition("e2","fin"),Transition("e3","st2")),StateType.STATE))
	  states should contain(State("st2",Nil,List(Transition("e2","fin")),StateType.STATE))
	  states should contain(State("fin",Nil,Nil,StateType.END))
	}

	test("ParseException should be raised if there is parse exception"){
	  val text = """
	    engine e1
			  version 1.0
	    
			  events
			  	next
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stop
			  end
	    end
	    
	    engine e2
			  events
			  	next
			  end
	    
			  start begin
			  	next => end
			  end
	    
			  finish end
			  end
	    end
	    """
	  val e = evaluating(FfseParser.parse(text)) should produce[ParseException]
	}

	test("ValidationException should be raised if there is parse is valid, but validation fails"){
	  val text = """
	    engine e1
			  version 1.0
	    
			  events
			  	nexta
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stop
			  end
	    end
	    
	    engine e2
			  version 2.0
	    
			  events
			  	next
			  end
	    
			  start begin
			  	next => stop
			  end
	    
			  finish stopit
			  end
	    end
	    """
	  val e = evaluating(FfseParser.parse(text)) should produce[ValidationException]
	}
	
	test("execption if engine name duplicated"){}
}