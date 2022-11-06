import scala.collection.mutable.ArrayBuffer
import java.util.HashSet

object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Neutral = Value

    def relativeFloorToDirection(rf: Int) = {
                if (rf < 0) {
                    Direction.Down
                } else if (rf > 0) {
                    Direction.Up
                } else {
                    Direction.Neutral
                }
    }
}

class Elevator(val maxCapacity: Int, val initialFloor:Int = 0) {
    var currentFloor: Int = initialFloor;
    var people: scala.collection.mutable.Set[Person] = scala.collection.mutable.Set[Person]();
    def nOfPassengers = people.size;
    def isEmpty = this.people.size == 0;

    def directionToGo:Direction.Value = {
        //if empty Neutral, else UP/DOWN, maybe better to use as Int than enum? (but we need enum on person.state)
        if(this.isEmpty) {
            //This branch should not be reached
            Direction.Neutral;
        } else {
            
            val wantedDirections = people.map(p => p.goal.get.floor)        //get goal floor
                                     .map(tf => tf - this.currentFloor)     //get get relative floor
                                     .map(Direction.relativeFloorToDirection)         //get direction

            val dirUp = wantedDirections.contains(Direction.Up)
            val dirDown = wantedDirections.contains(Direction.Down)
            assert( !(dirUp && dirDown) ); //Elevator should not contain people wanting to go different directions

            if(dirUp){
                Direction.Up
            } else if (dirDown) {
                Direction.Down
            } else {
                //Elevator is already at target floor
                Direction.Neutral
            }
        }
    }

    def addPerson(person:Person): Boolean = {
        if(this.nOfPassengers < this.maxCapacity){
            people.add(person)
            person.state = PersonState.InElevator
            true
        } else {
            false
        }
    }

    def releasePeople() = {
        val peopleToRelease = this.people.filter(x=>x.goal.get.floor==currentFloor)
        for (person <- peopleToRelease){
            person.floor=this.currentFloor
            person.state = PersonState.Walking
            people.remove(person)
        }
    }
}
