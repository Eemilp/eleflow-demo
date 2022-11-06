class Place (coords:(Int,Int), val appealsTo: PersonType.Value = PersonType.getRandom()){
    val x = coords._1
    val floor = coords._2
}
