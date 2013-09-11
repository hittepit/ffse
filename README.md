TODO

+ ajouter une description optionnelle pour
	- machine
	- events
	- commands
	- state

Example definition (not correct)

    engine test
    	version 1.0.0

        events
          doorClosed
          drawOpened
          lightOn   
          doorOpened
          panelClosed
        end

        commands
          unlockPanel => be.hittepit.executor.Test1
          lockPanel => be.hittepit.executor.Test2
          lockDoor => be.hittepit.executor.Test3
          unlockDoor => be.hittepit.executor.Test4
        end

    	start startState
	    	startEvent => active
	    end
	
    	finish finsihState
	    end
	
        state idle
          actions {unlockDoor lockPanel}
          doorClosed => active
        end

        state active
          drawOpened => waitingForLight
          lightOn    => waitingForDraw
        end

        state waitingForLight
          lightOn => unlockedPanel
        end

        state waitingForDraw
          drawOpened => unlockedPanel
        end

        state unlockedPanel
          actions {unlockPanel lockDoor}
          panelClosed => idle
        end
    end

http://blog.efftinge.de/2012/05/implementing-fowlers-state-machine-dsl.html
