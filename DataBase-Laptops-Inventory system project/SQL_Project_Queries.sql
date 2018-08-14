
USE newDB


/****** Object:  Table [dbo].[Customer]    Script Date: 12/13/2017 2:16:49 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[login](
	[UserName] [varchar](40) NOT NULL,
	[Password] [varchar](40) NOT NULL,
	[type] [varchar](35) NULL,
PRIMARY KEY CLUSTERED 
(
	[UserName] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO


CREATE TABLE [dbo].[Customer](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[FirstName] [varchar](20) NOT NULL,
	[LastName] [varchar](20) NULL,
	[DateOfBith] [date] NOT NULL,
	[Gender] [varchar](20) NOT NULL,
	[Address] [varchar](200) NOT NULL,
	[Cell_No] [bigint] NOT NULL,
	[UserName] [varchar](40) NOT NULL,
	[Password] [varchar](40) NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Customer]  WITH CHECK ADD  CONSTRAINT [New_FK_Constraint] FOREIGN KEY([UserName])
REFERENCES [dbo].[login] ([UserName])
ON UPDATE CASCADE
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[Customer] CHECK CONSTRAINT [New_FK_Constraint]
GO

---Employee Table


CREATE TABLE [dbo].[Employee](
	[EmpID] [int] IDENTITY(1,1) NOT NULL,
	[FName] [varchar](40) NOT NULL,
	[LName] [varchar](40) NOT NULL,
	[CellNo] [varchar](40) NOT NULL,
	[Designation] [varchar](40) NOT NULL,
	[DateOfJoining] [varchar](40) NOT NULL,
	[Salary] [int] NOT NULL,
	[ContractYears] [int] NOT NULL,
	[DateOfBirth] [date] NULL,
	[gender] [varchar](31) NOT NULL,
	[UserName] [varchar](31) NOT NULL,
	[Password] [varchar](41) NULL,
 CONSTRAINT [UPKCL_Employee] PRIMARY KEY CLUSTERED 
(
	[EmpID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]


GO



CREATE TABLE [dbo].[Laptops_Spec](
	[LaptopID] [int] IDENTITY(1,1) NOT NULL,
	[Processor] [varchar](30) NOT NULL,
	[Ram] [varchar](30) NOT NULL,
	[Memory] [varchar](40) NOT NULL,
	[Generation] [varchar](40) NOT NULL,
	[LaptopName] [varchar](50) NOT NULL,
	[Price] [float] NOT NULL,
 CONSTRAINT [LS_Spec] PRIMARY KEY CLUSTERED 
(
	[LaptopID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO



CREATE TABLE [dbo].[Shops](
	[ShopID] [int] IDENTITY(1,1) NOT NULL,
	[ShopName] [varchar](20) NOT NULL,
	[Address] [varchar](200) NOT NULL,
	[Cell_No] [bigint] NOT NULL,
	[reg_Date] [datetime] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[ShopID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

CREATE TABLE [dbo].[Laptops](
	[LaptopID] [int] NOT NULL,
	[ShopID] [int] NOT NULL,
	[type] [varchar](200) NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Laptops]  WITH CHECK ADD FOREIGN KEY([LaptopID])
REFERENCES [dbo].[Laptops_Spec] ([LaptopID])
GO

ALTER TABLE [dbo].[Laptops]  WITH CHECK ADD FOREIGN KEY([ShopID])
REFERENCES [dbo].[Shops] ([ShopID])
GO









CREATE TABLE [dbo].[Orders](
	[OrderID] [int] IDENTITY(1,1) NOT NULL,
	[CustomerID] [int] NOT NULL,
	[ShippedDate] [datetime] NOT NULL,
	[ShipAddress] [varchar](100) NOT NULL,
	[ShippedCity] [varchar](40) NOT NULL,
	[ShippedCountry] [varchar](40) NOT NULL,
	[OrderDateTime] [datetime] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[OrderID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Orders]  WITH CHECK ADD FOREIGN KEY([CustomerID])
REFERENCES [dbo].[Customer] ([ID])
GO


CREATE TABLE [dbo].[OrderDetail](
	[OrderID] [int] NOT NULL,
	[ProductID] [int] NOT NULL,
	[Quantity] [int] NOT NULL,
	[UnitPrice] [float] NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[OrderDetail]  WITH CHECK ADD FOREIGN KEY([OrderID])
REFERENCES [dbo].[Orders] ([OrderID])
GO

ALTER TABLE [dbo].[OrderDetail]  WITH CHECK ADD FOREIGN KEY([ProductID])
REFERENCES [dbo].[Laptops_Spec] ([LaptopID])
GO









CREATE TABLE [dbo].[Parts](
	[PartID] [int] IDENTITY(1,1) NOT NULL,
	[PartName] [varchar](50) NOT NULL,
	[Company] [varchar](30) NULL,
	[Country] [varchar](20) NOT NULL,
	[Description] [varchar](50) NOT NULL,
	[color] [varchar](20) NOT NULL,
	[Price] [float] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[PartID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO



CREATE TABLE [dbo].[Shop_Emp](
	[EmpID] [int] NOT NULL,
	[ShopID] [int] NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Shop_Emp]  WITH CHECK ADD FOREIGN KEY([EmpID])
REFERENCES [dbo].[Employee] ([EmpID])
GO

ALTER TABLE [dbo].[Shop_Emp]  WITH CHECK ADD FOREIGN KEY([ShopID])
REFERENCES [dbo].[Shops] ([ShopID])
GO



CREATE TABLE [dbo].[Shop_Part](
	[PartID] [int] NOT NULL,
	[ShopID] [int] NOT NULL,
	[type] [varchar](200) NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Shop_Part]  WITH CHECK ADD FOREIGN KEY([PartID])
REFERENCES [dbo].[Parts] ([PartID])
GO

ALTER TABLE [dbo].[Shop_Part]  WITH CHECK ADD FOREIGN KEY([ShopID])
REFERENCES [dbo].[Shops] ([ShopID])
GO



CREATE TABLE [dbo].[Suppliers](
	[Supplier_ID] [int] IDENTITY(1,1) NOT NULL,
	[FirstName] [varchar](20) NOT NULL,
	[LastName] [varchar](20) NULL,
	[Country] [varchar](20) NOT NULL,
	[Title] [varchar](20) NOT NULL,
	[Address] [varchar](20) NOT NULL,
	[Contact_No] [varchar](20) NULL,
PRIMARY KEY CLUSTERED 
(
	[Supplier_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO



CREATE TABLE [dbo].[Shop_Suppliers](
	[Shop_ID] [int] NOT NULL,
	[Supplier_ID] [int] NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Shop_Suppliers]  WITH CHECK ADD FOREIGN KEY([Shop_ID])
REFERENCES [dbo].[Shops] ([ShopID])
GO

ALTER TABLE [dbo].[Shop_Suppliers]  WITH CHECK ADD FOREIGN KEY([Supplier_ID])
REFERENCES [dbo].[Suppliers] ([Supplier_ID])
GO


CREATE TABLE [dbo].[ShoppingCart](
	[SCProduct_ID] [int] IDENTITY(1,1) NOT NULL,
	[CustomerID] [int] NOT NULL,
	[ProductName] [varchar](20) NULL,
	[Type] [varchar](20) NOT NULL,
	[Price] [float] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[SCProduct_ID] ASC,
	[CustomerID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[ShoppingCart]  WITH CHECK ADD FOREIGN KEY([CustomerID])
REFERENCES [dbo].[Customer] ([ID])
GO



-----------------------===========================================--------------------------------

Insert into login 
(
UserName,
Password,
type
)
values 
('Admin',
'Admin',
'Admin'
)



insert into login 
(UserName,Password,type)
values ('SL221','Pass','Customer')

Insert into customer 
(FirstName,
LastName,
DateOfBith,
Gender,
Address,
Cell_No,
UserName,
Password)
values (
'Sherlock',
'Holmes',
'1994-12-11',
'male',
'221B Baker Street',
'+9234455667',
'SL221',
'Pass')



insert into Laptops_Spec
(Processor,Ram,Memory,Generation,LaptopName,Price)
values ('2.7 GHz',	'4 GB',	'250 GB',	'4G',	'Asus-2224',	'28000')

insert into Laptops_Spec
(Processor,Ram,Memory,Generation,LaptopName,Price)
values ('3.7 GHz',	'8 GB',	'400 GB',	'4G',	'Asus-A24',	'48000')


insert into Shops 
(ShopName,Address,Cell_No,reg_Date)
values ('TechHouse','Street 2 floor 3 Technocity','+92345678754','2011-12-12')


insert into Laptops
(LaptopID,ShopID,type)
values (2,1,'Asus')

insert into Laptops
(LaptopID,ShopID,type)
values (1,1,'Asus')



insert Suppliers
(Firstname,LastName,Country,Title,Address,Contact_No)
values ('Shan','Masood','Pakistan','Professional','Defence phase 4','+923425678')




