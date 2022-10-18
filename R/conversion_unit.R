conversion_unit <- function (units, kg=NA, other=NA, reclassification_packaging){
  conversion <- NA
  if(units == "Other"){
    conv <- reclassification_packaging[reclassification_packaging$other_packaging==other,]
    return(c(conv$conversion_new, conv$kg_new))
  }
  kg = as.numeric(kg)
  if(units == "Mudu" | units == "Mudu/Kwano" | units == "Mudu/Kwano/Tiyya"){
    conversion = 0.4
  }else if(units == "Bag"){
    conversion = 0.01
  }else if(units == "10kg"){
    conversion = 0.1
  }else if(units == "25kg"){
    conversion = 0.04
  }else if(units == "50kg"){
    conversion = 0.02
  }else if(units == "100kg" | units =="100kg_Bag"){
    conversion = 0.01
  }else if(units == "Kg" | units == "1kg"){
    if(is.na(kg)){
      conversion = 1
    }else{
      conversion = 1/kg
    }
  }
  return(c(conversion, NA))
}
