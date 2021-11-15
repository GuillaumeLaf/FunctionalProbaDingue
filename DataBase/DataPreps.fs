namespace DataBase

open System

module DataPreps = 
    let formatTime (timeString:string) = 
        let splittedString = timeString.Split ' '
        let dateComps = splittedString.[0].Split '/'
        let timeComps = splittedString.[1].Split ':'
        new DateTime(int dateComps.[2],int dateComps.[0],int dateComps.[1],int timeComps.[0],int timeComps.[1],int timeComps.[2])

