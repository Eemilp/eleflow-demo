

class Analyser(val sim: Simulation) {
    var peopleLocationVisits:Map[Person,scala.collection.mutable.Map[PersonType.Value,Int]] = sim.people.map(x=>(x,PersonType.values.map(y=>(y,1)).to(scala.collection.mutable.Map))).toMap

    def update() = {
        for(person <- sim.people.filter(x=>x.state==PersonState.AtLocation)){
            peopleLocationVisits(person)(person.currentLocation.get.appealsTo)+=1
        }
    }
    def groupedCorrectness():Map[PersonType.Value,Int]={        
        val percents:Map[Person,scala.collection.mutable.Map[PersonType.Value,Float]] = peopleLocationVisits.map(x=>(x._1,
                                                    x._2.map(typeAndInt=>(typeAndInt._1,typeAndInt._2.toFloat/x._2.map(_._2).sum.toFloat))))
        val meanPercent:Map[PersonType.Value, Float] = percents.map(_._2).flatten.groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum/x._2.size))

        val personPredictions: Map[Person, PersonType.Value] = percents.map(x => (x._1, x._2.map(pt => (pt._1, pt._2 - meanPercent(pt._1))   ).maxBy(_._2)._1    ))

        val correctPredictions = personPredictions.map(x => (x._1, x._2 == x._1.personType))

        val groupedCorrectness = correctPredictions.groupBy(_._1.personType).map(x => (x._1, x._2.count(_._2)))

        groupedCorrectness
    }
    def totalAmountOfPeople():Map[PersonType.Value,Int] = sim.people.groupBy(_.personType).map(p => (p._1, p._2.size))
}
