/****** Script for SelectTopNRows command from SSMS  ******/
USE PortfolioProject


  SELECT *
  FROM PortfolioProject..NashvilleHousing;

  -- Standardize Date Format

  SELECT SaleDate, CONVERT(Date, SaleDate)
  FROM PortfolioProject..NashvilleHousing;

  ALTER TABLE NashvilleHousing
  ADD SaleDateConverted Date;

  UPDATE NashvilleHousing
  SET SaleDateConverted= CONVERT(Date,SaleDate);

  SELECT SaleDateConverted
  FROM PortfolioProject..NashvilleHousing;

  SELECT *
  FROM PortfolioProject..NashvilleHousing;

 
 ---- Populate Property Address

  SELECT PropertyAddress
  FROM PortfolioProject..NashvilleHousing
  WHERE PropertyAddress is null;
  
  SELECT ParcelID
  FROM PortfolioProject..NashvilleHousing
  WHERE ParcelID is null;
  ---since parcelId is unique for each property address and has no NULL value, we can use a self join to populate the missing values in the property address from the ParcelID

  SELECT *
  FROM PortfolioProject..NashvilleHousing
  --WHERE PropertyAddress is null;
  ORDER BY ParcelID

  ---SELF JOIN

  SELECT a.ParcelID, a.PropertyAddress,b.PropertyAddress,ISNULL(a.PropertyAddress, b.PropertyAddress)
  FROM PortfolioProject..NashvilleHousing a
  JOIN
  PortfolioProject..NashvilleHousing b
  ON
  a.ParcelID=b.ParcelID
  AND
  a.[UniqueID ] <> b.[UniqueID]
  WHERE a.PropertyAddress IS NULL

  UPDATE a
  SET PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
  FROM PortfolioProject..NashvilleHousing a
  JOIN
  PortfolioProject..NashvilleHousing b
  ON
  a.ParcelID=b.ParcelID
  AND
  a.[UniqueID ] <> b.[UniqueID]
  WHERE a.PropertyAddress IS NULL


  ---- Spliting the PropertyAddress into individual Columns (Address, City, State)
SELECT PropertyAddress
FROM PortfolioProject..NashvilleHousing

SELECT 
SUBSTRING(PropertyAddress, 1, CHARINDEX(',',PropertyAddress) -1) AS Address,
SUBSTRING(PropertyAddress, CHARINDEX(',',PropertyAddress) +1, LEN(PropertyAddress)) AS Address
FROM PortfolioProject..NashvilleHousing;


ALTER TABLE NashvilleHousing
ADD PropertySplitAdd NVARCHAR(255);

UPDATE NashvilleHousing
SET PropertySplitAdd= SUBSTRING(PropertyAddress, 1, CHARINDEX(',',PropertyAddress) -1);

ALTER TABLE NashvilleHousing
ADD PropertySplitCity NVARCHAR(255);

UPDATE NashvilleHousing
SET PropertySplitCity= SUBSTRING(PropertyAddress, CHARINDEX(',',PropertyAddress) +1, LEN(PropertyAddress))

SELECT *
FROM PortfolioProject..NashvilleHousing

---Using ParseName to split address column

SELECT OwnerAddress
FROM PortfolioProject..NashvilleHousing


SELECT 
PARSENAME(REPLACE(OwnerAddress,',', '.'),3),
PARSENAME(REPLACE(OwnerAddress,',', '.'),2),
PARSENAME(REPLACE(OwnerAddress,',', '.'),1)
FROM PortfolioProject..NashvilleHousing;

ALTER TABLE NashvilleHousing
ADD Owner_Add NVARCHAR(255);

UPDATE NashvilleHousing
SET Owner_Add=PARSENAME(REPLACE(OwnerAddress,',', '.'), 3);

ALTER TABLE NashvilleHousing
ADD owner_city NVARCHAR(255);

UPDATE NashvilleHousing
SET owner_city=PARSENAME(REPLACE(OwnerAddress,',','.'), 2);

ALTER TABLE NashvilleHousing
ADD owner_state NVARCHAR(255);

UPDATE NashvilleHousing
SET owner_state=PARSENAME(REPLACE(OwnerAddress,',','.'),1);

------- CHANGE YAND N to Yes and No in SoldAsVacant

SELECT DISTINCT (SoldAsVacant),COUNT(SoldAsVacant) AS Cnt_Of_Sold
FROM PortfolioProject..NashvilleHousing
GROUP BY SoldAsVacant
ORDER BY 2;

SELECT SoldAsVacant,
	CASE WHEN SoldAsVacant='Y'THEN 'Yes'
		WHEN SoldAsVacant='N'THEN 'NO'
		ELSE SoldAsVacant
		END
FROM PortfolioProject..NashvilleHousing


UPDATE NashvilleHousing
SET SoldAsVacant= CASE WHEN SoldAsVacant='Y'THEN 'Yes'
		WHEN SoldAsVacant='N'THEN 'NO'
		ELSE SoldAsVacant
		END;


----Remove Duplicate Data

SELECT *
FROM PortfolioProject..NashvilleHousing

WITH RowNumCTE AS(
SELECT *,
	ROW_NUMBER() OVER (
	PARTITION BY ParcelID,
				PropertyAddress,
				SalePrice,
				SaleDate,
				LegalReference
				ORDER BY
					UNIQUEID
					) row_num

FROM PortfolioProject..NashvilleHousing
)
SELECT *
FROM RowNumCTE
WHERE
row_num >1
ORDER BY PropertyAddress

---Delete

WITH RowNumCTE AS(
SELECT *,
	ROW_NUMBER() OVER (
	PARTITION BY ParcelID,
				PropertyAddress,
				SalePrice,
				SaleDate,
				LegalReference
				ORDER BY
					UNIQUEID
					) row_num

FROM PortfolioProject..NashvilleHousing
)

DELETE
FROM RowNumCTE
WHERE
row_num >1

----Remove irrevant columns

ALTER TABLE NashvilleHousing
DROP COLUMN
	OwnerAddress,
	PropertyAddress,
	TaxDistrict;

ALTER TABLE NashvilleHousing
DROP COLUMN
	SaleDate;

SELECT *
FROM PortfolioProject..NashvilleHousing