import scala.collection.mutable.Queue
import java.util.Dictionary
class ElevatorGroup(val xlocation: Int, val maxCapacity:Int = 3 ,val elevatorN:Int = 2) {
    // These people are waiting for the elevator
    val elevators: Vector[Elevator] = Range(0,elevatorN).map(x=> new Elevator(maxCapacity)).toVector

    var waitingPeople: Queue[Person] = Queue[Person]();

    private def addPersonToElevator(person: Person, elevator: Elevator) {
        if(elevator.addPerson(person)){
            this.waitingPeople.removeAll(_ == person)
        }
    }

    def update() = {
        /*
            NOTE: People are always borading elevator at the start of the turn and released at the end of the turn
        */
        for(elevator <- elevators){
            var goneForFirst = false
            if(elevator.isEmpty && waitingPeople.nonEmpty){
                //Pick up people if you can (only ones going in same diraction, choose by majority) if there are some
                //Go where you are needed or in the direction that people wanted
                var targetFloor = -1
                var guyToTake = waitingPeople.head
                if(goneForFirst==true){guyToTake=waitingPeople.last}
                goneForFirst = true

                if(guyToTake.floor==elevator.currentFloor) targetFloor=guyToTake.goal.get.floor
                else targetFloor = guyToTake.floor
                val targetDirection = Direction.relativeFloorToDirection(targetFloor - elevator.currentFloor)
                val peopleToBeTakenOn = waitingPeople.filter(_.floor == elevator.currentFloor)
                                                     .filter(p => Direction.relativeFloorToDirection(p.goal.get.floor - elevator.currentFloor) == targetDirection)
                //take people on
                peopleToBeTakenOn.foreach(p => 
                        addPersonToElevator(p, elevator)
                ) 
                elevator.currentFloor += (targetDirection match {
                    case Direction.Up => 1
                    case Direction.Down => -1
                    case _ => 0
                })
                elevator.releasePeople()

            } else if(waitingPeople.nonEmpty) {

                val peopleToBeTakenOn = waitingPeople.filter(_.floor == elevator.currentFloor)
                                                     .filter(p => Direction.relativeFloorToDirection(p.goal.get.floor - elevator.currentFloor) == elevator.directionToGo)
                //take people on
                peopleToBeTakenOn.foreach(p => 
                        addPersonToElevator(p, elevator)
                )

                //a match is used here because enum
                elevator.currentFloor += (elevator.directionToGo match {
                    case Direction.Up => 1
                    case Direction.Down => -1
                    case _ => 0
                })
                elevator.releasePeople()
            }
            //onboard people from waiting people of the given floor = waitingpeople.filter....
            //elevator.addPerson
        }        
    }
}
