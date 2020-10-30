nodes400 <- XML::getNodeSet(XML::xmlParse("reports/lcf_400_doc.xml"), "//seg")
names400 <- sapply(nodes400, XML::xmlGetAttr, "id")
nodes400 <- sapply(nodes400, XML::getChildrenStrings)
names(nodes400) <- names400


nodes100 <- XML::getNodeSet(XML::xmlParse("reports/lcf_100_doc.xml"), "//seg")
names100 <- sapply(nodes100, XML::xmlGetAttr, "id")
nodes100 <- sapply(nodes100, XML::getChildrenStrings)
names(nodes100) <- gsub("b\\." ,"a\\.", names100)

manual.sents <- c(nodes100, nodes400)
manual.sents <- sapply(manual.sents, trimws)

save(manual.sents, file = "reports/manual.sents.rdata")
