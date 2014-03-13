Pathway_Link <- function(x){
srv=SOAPServer("http://www.wikipathways.org/wpi/webservice/webservice.php");
a = .SOAP(srv, "findPathwaysByText", query=x, species="Homo sapiens", action=I("findPathwaysByText"), handlers=NULL);
b = xmlParse(a$content, asText=TRUE)
resultNodes = getNodeSet(b, "//*[local-name()='result']")
for(node in resultNodes) {
	children = xmlChildren(node, addNames= TRUE)
        url = xmlValue(children$url)
	#name = xmlValue(children$name);
	#species = xmlValue(children$species);
        print(paste(url, sep=""))
	
}
}
