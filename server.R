shinyServer(function(input,output){
  require(DT)
  require(shiny)
  require(dplyr)
  require(ggplot2)
  require(readxl)
  require(googleVis)
  require(RODBC)
  #require(gdata)
  #Functions
  
  ALL_Select=function(table,column,crit){
    if (is.null(crit)){
      return(table)
    }else{
      if (crit=='ALL'){
        return(table)
      }else{
        return(table[table[,column]==crit,])
      }
    }
  }
  
  
  setwd("~/Warehouse Report")
  
  
  #Warehouse in 20
  Warehouse_in_20=read_excel('Warehouse_in_20.xlsx')
  Warehouse_in_20=Warehouse_in_20[!apply(Warehouse_in_20,1,FUN = function(x) all(is.na(x))),]
  
  # Sales Section
  Daily_Sales=read_excel('Sales By Origins By Sku (20).xlsx')
  Daily_Sales=Daily_Sales[!apply(Daily_Sales,1,FUN=function(x) sum(!is.na(x))<10),]
  Daily_Sales=Daily_Sales[,c('Promo \r\nComp','Region','Origin Name','Date','Supplier \r\nCode','Sku','Group Style','Description','Product Category','Features','Cut/\r\nCnt_Cut','Color/\r\nCnt_Color','Clarity/\r\nCnt_Clarity','Ct Wt/\r\nCnt Ct Wt','Qty','Sold','Retail','Cost','Employee(s) Name')]
  
  names(Daily_Sales)=c('Promo','Region','Origin','Date','Supplier','Sku','Group Style','Description','Product Category','Features','Cut','Color','Clarity','Ct','Qty_','Sold_','Retail_','Cost_','Employee')
  
    # Make a copy
  Daily_Sales_With_Promo=Daily_Sales
  
  Daily_Sales=Daily_Sales %>% group_by(Region,Date,Supplier,Sku,`Group Style`,Description,`Product Category`,Features,Cut,Color,Clarity,Ct) %>% summarise(Qty=sum(Qty_),Sold=sum(Sold_),Retail=sum(Retail_),Cost=sum(Cost_))
  
  Daily_Sales['Sales Margin']=Daily_Sales$Sold/Daily_Sales$Cost
  Daily_Sales['Discount']=1-Daily_Sales$Sold/Daily_Sales$Retail
  
  Daily_Sales$Date=as.Date(Daily_Sales$Date,origin = "1898-12-30")
  
  
  Daily_Sales['tag']=sapply(Daily_Sales$Features,FUN=function(x) {
    if (grepl('ERAN',x)){
      return(0)
    }else if(grepl('CLRN',x)){
      return(1)
    }else if(grepl('(SETP)|(TAT)',x)){
      return(2)
    }else{
      return(3)
    }
  })
  
  # Daily Sales Summary
    #Text
  Greeting_text='
  Good Morning Albert, 
  <br/>
  <br/>
  Below is the sales summary from 01-10-2018
  <br/>
  <br/>
  See total sales recap below:
  <b>
  '
  Summary_text_1=''
  
  Daily_Sales_Summary_With_Promo=Daily_Sales_With_Promo %>% group_by(Promo) %>% summarise(Qty=sum(Qty_),Sold=sum(Sold_))
  
  order=c('OBM','PPI','RMP','HAB','OTHERS')

  Daily_Sales_Summary_With_Promo=Daily_Sales_Summary_With_Promo[match(order,Daily_Sales_Summary_With_Promo$Promo),]
  
  for (i in 1:dim(Daily_Sales_Summary_With_Promo)[1]){
    Summary_text_1=paste(Summary_text_1,paste(Daily_Sales_Summary_With_Promo[i,1],' total ',as.character(Daily_Sales_Summary_With_Promo[i,2]),' sales total $',format(as.integer(Daily_Sales_Summary_With_Promo[i,3]),big.mark=',',nsmall = 2)
                                          ,sep=''),sep='<br/><br/>')
  }
  
  Summary_text_1=paste(Summary_text_1,paste0('Total is <font size="5">$',format(as.integer(sum(Daily_Sales_Summary_With_Promo$Sold)),big.mark=',',nsmall = 2),'</b></font>'),sep='<br/><br/>')
  
  Big_Sales_text='
  <br/>
  <font size="5">
  <b>
  The Large Sales Were:
  </b>
  </font>
  '
  
  for (i in 1:dim(Daily_Sales_With_Promo)[1]){
    if (Daily_Sales_With_Promo$Sold_[i]>25000){
      big_sale_origin=paste0('<b>',Daily_Sales_With_Promo$Promo[i],' - ',Daily_Sales_With_Promo$Origin[i],'</b>')
      if(Daily_Sales_With_Promo$`Product Category`[i]=='JEWELRY'){
        big_sale_detail=paste0('In ',Daily_Sales_With_Promo$Region[i],', ',Daily_Sales_With_Promo$Employee[i],' sold, <u>',Daily_Sales_With_Promo$Cut[i],' ',Daily_Sales_With_Promo$Color[i],' ',Daily_Sales_With_Promo$Clarity[i],' ',as.character(Daily_Sales_With_Promo$Ct[i]), ' ', tail(strsplit(Daily_Sales_With_Promo$Description[i],split=" ")[[1]],1),'</u> for $', format(Daily_Sales_With_Promo$Sold_[i],big.mark=',',nsmall = 2),' cost $', format(Daily_Sales_With_Promo$Cost_[i],big.mark=',',nsmall = 2))
      }else if(Daily_Sales_With_Promo$`Product Category`[i]=='WATCHES'){
        big_sale_detail=paste0('In ',Daily_Sales_With_Promo$Region[i],', ',Daily_Sales_With_Promo$Employee[i],' sold, <u>', 'Hublot','</u> for $', format(Daily_Sales_With_Promo$Sold_[i],big.mark=',',nsmall = 2),' cost $', format(Daily_Sales_With_Promo$Cost_[i],big.mark=',',nsmall = 2))
      }else {
        big_sale_detail=paste0('In ',Daily_Sales_With_Promo$Region[i],', ',Daily_Sales_With_Promo$Employee[i],' sold, <u>',Daily_Sales_With_Promo$Cut[i],' ',Daily_Sales_With_Promo$Color[i],' ',Daily_Sales_With_Promo$Clarity[i],' ',as.character(Daily_Sales_With_Promo$Ct[i]), ' ', 'Diamond','</u> for $', format(Daily_Sales_With_Promo$Sold_[i],big.mark=',',nsmall = 2),' cost $', format(Daily_Sales_With_Promo$Cost_[i],big.mark=',',nsmall = 2))
      }
      big_sale=paste(big_sale_origin,big_sale_detail,sep='<br/>')
      Big_Sales_text=paste(Big_Sales_text,big_sale,sep='<br/><br/>')
    }
  }
  
  output$Daily_Sales_text_1=renderUI(HTML(paste0(Greeting_text,Summary_text_1)))
  output$Daily_Sales_text_2=renderUI(HTML(paste0(Big_Sales_text)))
  
    # Graphs
  output$Daily_Sales_Graph_1=renderGvis(gvisPieChart(Daily_Sales_Summary_With_Promo,options = list(
    title='Quantity'
  )))
  output$Daily_Sales_Graph_2=renderGvis(gvisPieChart(Daily_Sales_Summary_With_Promo[,c('Promo','Sold')],options = list(
    title='Sales'
  )))
    # by Group Style
  Daily_Sales_by_GroupStyle_table=datatable(Daily_Sales[Daily_Sales$Qty != 0, ] %>% group_by(`Group Style`) %>% summarise(Qty=sum(Qty),Total_Sales=sum(Sold),Total_Retail=sum(Retail),Total_Cost=sum(Cost)))
  output$Daily_Sales_by_GroupStyle=renderDataTable(Daily_Sales_by_GroupStyle_table
                                                   %>% formatCurrency(c('Total_Sales','Total_Retail','Total_Cost'),'$')
                                                   %>% formatStyle('Qty',target='row',backgroundColor=styleInterval(0,c('pink','white')))
                                                   )
  # by Region
  Daily_Sales_by_Region_table=datatable(Daily_Sales[Daily_Sales$Qty != 0, ] %>% group_by(Region) %>% summarise(Qty=sum(Qty),Total_Sales=sum(Sold),Total_Retail=sum(Retail),Total_Cost=sum(Cost)))
  output$Daily_Sales_by_Region=renderDataTable(Daily_Sales_by_Region_table
                                                   %>% formatCurrency(c('Total_Sales','Total_Retail','Total_Cost'),'$')
                                                   %>% formatStyle('Qty',target='row',backgroundColor=styleInterval(0,c('pink','white')))
  )  
  # Daily Sales Detail

  output$Diamond_Type_1=renderUI(
    selectInput('Diamond_Type_1','Select a product category',unique(Daily_Sales$`Product Category`))
  )
  
  output$Daily_Sales_1=DT::renderDataTable(datatable(Daily_Sales[Daily_Sales$Qty > 0 & Daily_Sales$`Product Category`==input$Diamond_Type_1,],
                                                     extensions = 'Buttons', 
                                                      options=list(pageLength=100,
                                                              lengthMenu = list(c(50, 100, -1), list('50', '100', 'All')),
                                                              columnDefs = list(list(visible=FALSE, targets=c(8,19))),
                                                              dom = 'Bfrtip',
                                                              buttons = 
                                                                list('copy', list(
                                                                  extend = 'collection',
                                                                  title='Sales Report',
                                                                  buttons = c('csv', 'excel', 'pdf'),
                                                                  text = 'Download'
                                                                )),
                                                              footerCallback= JS(
                                                                "function( tfoot, data, start, end, display ) {",
                                                                "var api = this.api(), data;",
                                                                "total = api.column( 14, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",
                                                                "total1 = api.column( 15, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",
                                                                "total2 = api.column( 16, { page: 'current'} ).data().reduce( function ( a, b ) {return a + b;} )",

                                                                "$( api.column( 14 ).footer() ).html(total.toFixed(2));
                                                                $( api.column( 15 ).footer() ).html(total1.toFixed(2));
                                                                $( api.column( 16 ).footer() ).html(total2.toFixed(2));
",
                                                                "}"
                                                              )))
                                       %>% formatCurrency(c('Sold','Retail','Cost'),'$')
                                       %>% formatRound('Sales Margin',digits=2)
                                       %>% formatPercentage('Discount',2)
                                       %>% formatStyle('Discount',backgroundColor=styleInterval(0.25,c('white','pink')))
                                       %>% formatStyle('Group Style','tag',backgroundColor=styleEqual(c(0,1,2,3),c('ORCHID', 'orange','pink','DARKKHAKI')),fontWeight='bold')
                                       )

  
  #Inventory Section
  
  #data=read.csv('export.csv')
  db=odbcConnect('Demo',uid = 'AlbertDept', pwd = 'Password123')
  #Query1=paste("SELECT * FROM InventoryByFeatureBySupplier", "WHERE AllFeatures LIKE '%COL'")
  data=sqlQuery(db,"SELECT * FROM InventoryByFeatureBySupplier WHERE AllFeatures LIKE '%COL%'")
  print(count(data))
  close(db)
  Looses=data[data$Supplier %in% c('D-P','D-L','D-S'),]
  Jewelrys=data[!(data$Supplier %in% c('D-P','D-L','D-S')),]
  
  Wired_Jewelry=Jewelrys[abs(Jewelrys$OnHandQty)!=1,]
  
  Wired_loose=Looses[Looses$OnHandQty%%1!=0,]
  Qty_looses=Looses %>% group_by(OnHandQty) %>% summarise(n=n())
  Suppliers=data %>% group_by(Supplier) %>% summarise()
  
  
  Normal_Loose=Looses[!(Looses %in% Wired_loose),]
  Normal_Jewelry=Jewelrys[!(Jewelrys %in% Wired_Jewelry)]
  
  
  Normal_Loose_in_20=Normal_Loose[Normal_Loose$Region==20,]
  Normal_Loose_in_Distribution=Normal_Loose[Normal_Loose$Region!=20,]
  
  Normal_Loose_in_20=inner_join(Normal_Loose_in_20,Warehouse_in_20,by='Store')
  Normal_Loose_in_20$Store=apply(Normal_Loose_in_20[,c('Store','Name')],1,FUN=function(x) paste(x[1],x[2],sep=' '))
  
  Miami_List=read_excel('Miami WareHouse.xlsx',col_names = FALSE)
  
  # Change Miami Warehouse
  
  for (i in 1:length(Normal_Jewelry)){
    if (Normal_Jewelry$Store[i] %in% Miami_List$X__1[!is.na(Miami_List$X__2)]){
      Normal_Jewelry$Region[i]=Miami_List$X__2[Miami_List$X__1==Normal_Jewelry$Store[i]]
    }  
  }
  
  for (i in 1:length(Normal_Loose_in_Distribution)){
    if (Normal_Loose_in_Distribution$Store[i] %in% Miami_List$X__1[!is.na(Miami_List$X__2)]){
      Normal_Loose_in_Distribution$Region[i]=Miami_List$X__2[Miami_List$X__1==Normal_Loose_in_Distribution$Store[i]]
    }  
  }
  
  Pivot_Jewelry= Normal_Jewelry %>% group_by(Region) %>% summarise(Quantity=sum(OnHandQty))
  Pivot_Loose=Normal_Loose %>% group_by(Region) %>% summarise(Quantity=sum(OnHandQty))
  Pivot_Loose_in_20=Normal_Loose_in_20 %>% group_by(Store) %>% summarise(Quantity=n())
  
  #Inventory Summary Page
  
  #Jewelry Count
  output$Pivot_Jewelry_1=renderTable(Pivot_Jewelry[1:7,],width='18%')
  output$Pivot_Jewelry_2=renderTable(Pivot_Jewelry[8:14,],width='18%')
  output$Pivot_Jewelry_3=renderTable(Pivot_Jewelry[15:21,],width='18%')
  output$Pivot_Jewelry_4=renderTable(Pivot_Jewelry[22:28,],width='18%')
  output$Pivot_Jewelry_5=renderTable(Pivot_Jewelry[29:35,],width='18%')
  #Loose Count
  output$Pivot_Loose_1=renderTable(Pivot_Loose[1:7,],width='18%')
  output$Pivot_Loose_2=renderTable(Pivot_Loose[8:14,],width='18%')
  output$Pivot_Loose_3=renderTable(Pivot_Loose[15:21,],width='18%')
  output$Pivot_Loose_4=renderTable(Pivot_Loose[22:28,],width='18%')
  output$Pivot_Loose_5=renderTable(Pivot_Loose[29:35,],width='18%')
  
  #Warehouse in 20 (New York)
  
  output$Pivot_Loose_in_20_1=renderTable(Pivot_Loose_in_20[1:10,],width='23%')
  output$Pivot_Loose_in_20_2=renderTable(Pivot_Loose_in_20[11:20,],width='23%')
  output$Pivot_Loose_in_20_3=renderTable(Pivot_Loose_in_20[21:30,],width='23%')
  output$Pivot_Loose_in_20_4=renderTable(Pivot_Loose_in_20[31:40,],width='23%')
  
  output$Store_Select_1=renderUI(
    selectInput("Store_1",'Select a store',Pivot_Loose_in_20$Store)
  )
  
  output$Detail_Loose_in_20=shiny::renderDataTable(Normal_Loose_in_20[Normal_Loose_in_20$Store==input$Store_1,c(11,14,15,17,18,1)],options = list(lengthMenu = list(c(25, 50, -1), list('25', '50', 'All'))))
  
  #Jewelry by Region Page
  
  output$Region_Select_1=renderUI(
    selectInput("Region_1",'Select a region',c('ALL',Pivot_Jewelry$Region),selected='ALL')
  )
  
  DataInput_1= reactive({
      d_table=ALL_Select(Normal_Jewelry,'Region',input$Region_1)%>% group_by(GroupStyle,AllFeatures) %>% summarise(Total_Quantity=sum(OnHandQty),Total_Retail=sum(TotalRetail),Total_Cost=sum(TotalCost))
      #d_table$GroupStyle=sapply(d_table$GroupStyle,as.character)
      #d_table$AllFeature=sapply(d_table$AllFeature,as.character)
      datatable(d_table, filter = 'top') %>% formatCurrency(c('Total_Retail','Total_Cost'),'$')
  })
  
  output$Jewelry_Inventory_by_Region=DT::renderDataTable(DataInput_1())
  
  #Loose Stone Inventory by Region Page
  
  output$Region_Select_2=renderUI(
    selectInput("Region_2",'Select a region',Pivot_Jewelry$Region)
  )
  
  #Loose
  
  output$Loose_Criteria_1=renderUI(
    selectInput('Criteria_1','Cut',unique(Normal_Loose$Cut))
  )
  
  output$Loose_Criteria_2=renderUI(
    selectInput('Criteria_2','Color',unique(Normal_Loose$Color))
  )
  
  output$Loose_Criteria_3=renderUI(
    selectInput('Criteria_3','Clarity',unique(Normal_Loose$Clarity))
  )
  
  output$Loose_Criteria_4=renderUI(
    sliderInput('Criteria_4','Weight',0,10,value=c(0,10),step=0.01)
    #sliderInput('Criteria_4','Weight',min(Normal_Loose$Gram_Wt),max(Normal_Loose$Gram_Wt))
  )
  
  output$Loose_Criteria_5=renderUI(
    textInput('Criteria_5','Features')
  )
  

  
  # Open Memos
  
  read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
  }
  mysheets <- read_excel_allsheets("S:/Factory Memos/Factory Memos.xlsx")
  
  Opens=mysheets[grepl(pattern = '(OPEN)|Open',names(mysheets))]
  
  col_names= c("Date_Sent","Line","Category","Memo_No","Item_No","SKU", "CUT","STYLE","METAL","CENTER_DIA.","COLOR","CLARITY","MM","COST","N_of_STN","2nd_Sku","Start_Date","Due_Date","Comment","Invoice","Invoice_Date")
  
  
  output$tables=renderUI({
    table_output_list=lapply(1:length(Opens),function(i){
      tablename = paste0('Open_Memos_',strsplit(names(Opens[i]),' ')[[1]][1])
      tagList(
      h3(strsplit(names(Opens[i]),' ')[[1]][1]),
      DT::dataTableOutput(tablename),
      hr()
      )
    })
    
    do.call(tagList, table_output_list)
  })
  
  for (i in 1:length(Opens)){
    local({
      my_i=i
      #print(names(Opens[my_i]))
      Vendor_Name=strsplit(names(Opens[my_i]),' ')[[1]][1]
      data=as.data.frame(Opens[my_i])
      if (dim(data)[1]>0){
        names(data)=col_names
        data$Memo_No=as.character(data$Memo_No)
        data2=data%>%group_by(Memo_No,STYLE)%>% summarise(Jewelrys=sum(!is.na(Item_No)),Stones=sum(N_of_STN),Start_Date=mean(Start_Date),Due_Date=mean(Due_Date)) %>% mutate(Start_Date=as.character(as.Date(as.POSIXct(Start_Date,origin='1960-01-01'))),Due_Date=as.character(as.Date(as.POSIXct(Due_Date,origin='1960-01-01'))),Days_Over_Due=Sys.Date()-as.Date(as.POSIXct(Due_Date,origin='1960-01-01')))
        data2$Vendor=Vendor_Name
        table_name=paste0('Open_Memos_',Vendor_Name)
        print(table_name)
        output[[table_name]]=DT::renderDataTable(datatable(data2[,c('Memo_No','STYLE','Jewelrys','Stones','Start_Date','Due_Date','Days_Over_Due')])
                                          %>% formatStyle('Days_Over_Due',backgroundColor=styleInterval(0,c('white','pink'))))
      }
    })
  }
  
  
  
  
  
})
  
