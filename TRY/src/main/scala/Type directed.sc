val raw = List(94300,75100,75001,67810)
val etape = Map(75-> "paris", 94->"Val de Marne",67-> "Seine et Marne ")
val next= Map(75 -> "IDF", 94 -> "IDF", 67->"Loire")

//raw.map(etape)
raw.map(_.toString.substring(0,2).toInt ).map(next)

val x = for (y<-(1900 to 2021);
     inf =y.toString.replaceAll(".$","0");
     max =y.toString.replaceAll(".$","9"))
    yield ( Map(y->("["+inf+","+max+"]")))

x.toList.flatten.toMap

val identifiers = List("idflux","dtflux","idvisiteur","idsession","adresse1","adresse2","adresse3","adresse4","nom_cp","prenom_cp","teldom","telport","email","idcnt","idclientpartenaire","nom_cs","prenom_cs","nom_ti","prenom_ti","num_sin1", "lien_sin1", "type_sin1", "date_sin1", "corpo_sin1", "responsabilite_sin1", "cause_sin1", "num_sin2", "lien_sin2", "type_sin2", "date_sin2", "corpo_sin2", "responsabilite_sin2", "cause_sin2", "num_sin3", "lien_sin3", "type_sin3", "date_sin3", "corpo_sin3", "responsabilite_sin3", "cause_sin3", "nbsin", "typereponse", "versionreponse", "nouvelleoffre", "coderejet", "formule_max", "lib_pack1", "montant_pack1", "lib_pack2", "montant_pack2", "lib_pack3", "montant_pack3", "lib_gar0", "libfranchise_gar0", "min_gar0", "max_gar0", "lib_gar1", "libfranchise_gar1", "min_gar1", "max_gar1", "lib_gar2", "libfranchise_gar2", "min_gar2", "max_gar2", "lib_gar3", "libfranchise_gar3", "min_gar3", "max_gar3", "lib_gar4", "libfranchise_gar4", "min_gar4", "max_gar4", "lib_gar5", "libfranchise_gar5", "min_gar5", "max_gar5", "lib_gar6", "libfranchise_gar6", "min_gar6", "max_gar6", "lib_gar7", "libfranchise_gar7", "min_gar7", "max_gar7", "lib_gar8", "libfranchise_gar8", "min_gar8", "max_gar8", "lib_gar9", "libfranchise_gar9", "min_gar9", "max_gar9", "prime_forfrac_2_0", "prime_forfrac_2_2", "prime_forfrac_3_0", "prime_forfrac_3_2", "prime_forfrac_4_0", "prime_forfrac_4_2", "prime_forfrac_5_0", "prime_forfrac_5_2", "exavisuexige")
//pairs(identifiers).flatten.distinct
val all_attributes=List("typeflux", "versionflux", "idcyber", "originebroker", "ebrokercb", "ipebroker", "codeinsee", "dtdebeff", "formule", "typfrac", "pack", "gpc", "nbveh_f", "resiliationcompagnie", "typeenvoi", "typfracdemande", "formuledemandee", "sexe_cp", "dtnaiss_cp", "sitmat_cp", "typeemail", "communicationda", "nbenfants", "dtnaissenf1", "dtnaissenf2", "dtnaissenf3", "typehabitation", "natureoccupant", "dureeoccupation", "dtpermis_cp", "modeobtpermis_cp", "paysobtpermis_cp", "profession_cp", "csp_cp", "usage_cp", "frequsage_cp", "distanceparcourue_cp", "cinseetra_cp", "permisconjointconcubin_cp", "cpautrevehicule_cp", "suspensionpermis_cp", "detailsuspensionpermis_cp", "motifsuspensionpermis_cp", "dureesuspensionpermis_cp", "nombresuspensionpermis_cp", "pointspermis_cp", "professionchoisie_cp", "interruption_cp", "cpanciencontrat_cp", "compagnieprecedente_cp", "dtderech_cp", "preavisresil_cp", "crm_cp", "dtacqcrm50_cp", "sexe_cs", "dtnaiss_cs", "sitmat_cs", "liencp_cs", "cpautreveh_cs", "dtpermis_cs", "csp_cs", "antecedentsassurance_cs", "suspensionpermis_cs", "detailsuspensionpermis_cs", "motifsuspensionpermis_cs", "dureesuspensionpermis_cs", "nombresuspensionpermis_cs", "pointspermis_cs", "interruption_cs", "cpanciencontrat_cs", "lienaveccpanciencontrat_cs", "existe_cs", "sexe_ti", "dtnaiss_ti", "sitmat_ti", "existe_ti", "liencp_ti", "codeauto", "immat_veh", "datepremieremec", "dateacquisition", "duredet", "natveh", "cinseegar", "typepark", "modacqui", "formule_prec", "type_remorque", "immat_remorque")
val uniquePairs = for {
    (x, idxX) <- all_attributes.zipWithIndex
    (y, idxY) <- all_attributes.zipWithIndex
    if idxX < idxY
} yield (x, y)


uniquePairs.size

def pairs (list:List[String]):List[(String,String)]=  list match {
    case Nil => Nil
    case head::Nil => Nil
    case head::tail =>{
        val head_list={for(col<-tail if head !=col ) yield (head,col)
        }
        head_list.toList ++(pairs(list.tail))}
}
pairs(all_attributes).contains(("suspensionpermis_cs","detailsuspensionpermis_cs"))



val triplets = for {
    (col1, indexcol1) <- all_attributes.zipWithIndex
    (col2, indexcol2) <- all_attributes.zipWithIndex
    (col3, indexcol3) <- all_attributes.zipWithIndex
    if indexcol1 < indexcol2 && indexcol2 < indexcol3
} yield (col1, col2, col3)

triplets.size



lazy val test = all_attributes.combinations(1)


while(test.hasNext){
    println(test.next())
}
