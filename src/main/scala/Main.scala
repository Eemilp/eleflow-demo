import org.scalajs.dom
import org.scalajs.dom.document
import scala.collection.immutable
import javax.swing.text.Document


object ElevsimApp {
    
    def main(args: Array[String]): Unit = {
        val sim = new Simulation()
        val analyser = new Analyser(sim)
        var paused:Boolean = true;
        var currentStep: Int = 0;

        sim.update()
        sim.getState()
        analyser.update()

        //Create grid container div
        //Create scala array to store grid elements in
        def createGridItem(ge:GridElement) = {
            val div = document.createElement("div")
            div.classList.add("grid-item")
            div.innerHTML=ge.people.toString()
            if(ge.isElevatorShaft && !ge.hasElevator) div.setAttribute("style","background-color:#CFD2CF")
            if(ge.isCabin) div.setAttribute("style","background-color:#CDFCF6")
            if(ge.hasElevator) div.setAttribute("style","background-color:#3AB4F2")
            if(ge.isPlace) div.setAttribute("style","background-color:#FA9494")
            div
        }

        

        val gridElements = sim.getState().map(x=>(x._1,createGridItem(x._2))).toMap
 
        val stateText = document.createElement("p")
        stateText.innerHTML = s"Simulation step ${currentStep}. <br>${sim.waitingPeople()} people waiting currently."

        val correctnessTextList = analyser.groupedCorrectness().map(x=>(x._1,document.createElement("p")))
        correctnessTextList.foreach(typeDiv => typeDiv._2.innerHTML=s"${analyser.groupedCorrectness()(typeDiv._1)}/${analyser.totalAmountOfPeople()(typeDiv._1)} of ${typeDiv._1.toString()} predicted correctly")

        dom.window.setInterval(()=> {
            if (!paused) {
            sim.update()
            analyser.update()
            correctnessTextList.foreach(typeDiv => typeDiv._2.innerHTML=s"${analyser.groupedCorrectness()(typeDiv._1)}/${analyser.totalAmountOfPeople()(typeDiv._1)} of ${typeDiv._1.toString()} predicted correctly")
            val state=sim.getState()
            for(xy_ge <- state){
                val elem=gridElements(xy_ge._1)
                elem.innerHTML=state(xy_ge._1).people.toString()
                if(xy_ge._2.isElevatorShaft) elem.removeAttribute("style")
                if(xy_ge._2.isElevatorShaft && !xy_ge._2.hasElevator) elem.setAttribute("style","background-color:#CFD2CF")
                if(xy_ge._2.hasElevator) elem.setAttribute("style","background-color:#3AB4F2")

            }
            currentStep += 1;
            stateText.innerHTML = s"Simulation step ${currentStep}. <br>${sim.waitingPeople()} people waiting currently."
        }},100)

        val pauseButton = document.createElement("button")
        pauseButton.textContent = "Start"
        pauseButton.addEventListener("click", { (e: dom.MouseEvent) =>{
            if(paused) {
                paused = false;
                pauseButton.textContent = "Pause"
            } else {
                paused = true;
                pauseButton.textContent = "Resume"
            }
        }})


        val customEventButton = document.createElement("button")
        customEventButton.textContent = "Start event at top left"
        customEventButton.addEventListener("click", { (e: dom.MouseEvent) => {
            sim.setCustomEvent();
            if(sim.customEventEnabled){
                customEventButton.textContent = "Stop event at top left"
            } else {
                customEventButton.textContent = "Start event at top left"
            }
        }})

        val eventButton = document.createElement("button")
        eventButton.textContent = "Start two random events"
        eventButton.addEventListener("click", { (e: dom.MouseEvent) => {
            sim.toggleEvent();
            if(sim.eventHappening){
                eventButton.textContent = "Clear all events"
            } else {
                eventButton.textContent = "Start two random events"
                customEventButton.textContent = "Start event at top left"
            }
        }})

        val legend = document.createElement("div")
        legend.innerHTML = "<span style=\"color: #FA9494\">■</span> Point of Interest, <span style=\"color: #CDFCF6\">■</span> Sleeping cabin, <span style=\"color: #3AB4F2\">■</span> Elevator"

        val predText = document.createElement("p")
        predText.innerHTML = "Additionally a model tries to classify each passenger into a similarly acting group based on their behaviour. Below you can see how the model is learning passenger behaviour in real time with the rising accuracy."

        val gridStyle = document.createElement("style")
        gridStyle.innerHTML = ".grid-container {" +
                              "   display: grid;" +
                              s"grid-template-columns: repeat(${sim.x.toString()},${100/sim.x}%);" +
                              "padding: 10px;" +
                              "}"

        val gridItemStyle = document.createElement("style")
        gridItemStyle.innerHTML = ".grid-item {" +
                                  "border: 1px solid rgba(0, 0, 0, 0.8);" +
                                  "padding: 4px;" +
                                  "font-size: 25px;" +
                                  "text-align: center;" +
                                  "}"


        val grid = document.createElement("div")
        grid.classList.add("grid-container")
        for(y<-Range(0,sim.y).reverse){
            for(x<-Range(0,sim.x)){
                grid.appendChild(gridElements((x,y)))
            }
        }        
        document.head.appendChild(gridStyle)
        document.head.appendChild(gridItemStyle)
        document.body.appendChild(pauseButton);
        document.body.appendChild(eventButton);
        document.body.appendChild(customEventButton);
        document.body.appendChild(stateText)
        document.body.appendChild(legend)
        document.body.appendChild(grid)
        document.body.appendChild(predText)
        correctnessTextList.foreach(x=>document.body.appendChild(x._2))
        }
}