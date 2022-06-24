USE PortfolioProject;

SELECT *
FROM PortfolioProject..CovidDeaths$;

--SELECT *
--FROM PortfolioProject..CovidVacc;

--SELECT DATA TO USE

SELECT Location,date,total_cases, new_cases,total_deaths,population
FROM PortfolioProject..CovidDeaths$
ORDER BY 1,2;

---Looking at Total Cases Vs Total Deaths
---shows the likelihood of dying if you contract covid in your country.

SELECT Location,date, total_deaths,total_cases, (total_deaths/total_cases) * 100 AS death_perc
FROM PortfolioProject..CovidDeaths$
WHERE Location like '%states%'
ORDER BY 1,2;

SELECT Location,date, Population,total_cases, (total_deaths/Population) * 100 AS Percentage_Pop_Infected
FROM PortfolioProject..CovidDeaths$




WHERE Location like '%states%'
ORDER BY 1,2;

---Looking at countries with Highest Infection rate compared to population

SELECT Location,Population, Max(total_cases) as HighestInfectionCount, Max((total_deaths/Population)*100) AS Percentage_Pop_Infected
FROM PortfolioProject..CovidDeaths$
GROUP BY Location, Population
ORDER BY Percentage_Pop_Infected DESC;


----BREAKDOWN BY CONTINENTS

SELECT *
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
order by 3,4
---Countries with the highest death_count per population

SELECT Location, Max(cast(total_deaths as int)) as TotalDeathCountDeath
FROM PortfolioProject..CovidDeaths$
WHERE continent is null
GROUP BY Location
ORDER BY TotalDeathCountDeath  DESC;

SELECT continent, Max(cast(total_deaths as int)) as TotalDeathCountDeath
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY continent
ORDER BY TotalDeathCountDeath  DESC;


--Global Numbers

SELECT date,SUM(new_cases) AS Total_Cases,SUM(CAST(new_deaths AS INT)) as Total_Deaths,(SUM(CAST(new_deaths AS INT))/SUM(new_cases))* 100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
GROUP BY date
ORDER BY 1,2;

SELECT SUM(new_cases) AS Total_Cases,SUM(CAST(new_deaths AS INT)) as Total_Deaths,(SUM(CAST(new_deaths AS INT))/SUM(new_cases))* 100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths$
WHERE continent is not null
---GROUP BY date
ORDER BY 1,2;



---JOINING THE TWO TABLES

--- Looking at Total Population Vs Vaccination
SELECT *
FROM PortfolioProject..CovidVacc dea
JOIN PortfolioProject..CovidDeaths$ vac
	ON
	dea.location=vac.location AND dea.date=vac.date;

SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations
FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
ORDER BY 2,3;

---

SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CONVERT(INT, vac.new_vaccinations))
FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
ORDER BY 2,3;


SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location) AS Total_New_Vac

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
ORDER BY 2,3;

---
SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location ORDER BY dea.location ,dea.Date) AS RollingPeopleVacc

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
ORDER BY 2,3;

---How many ppl in a country are vaccinated?
WITH PopVsVac(Continent,Location,Date,Population, New_vaccinations,RollingPeopleVaccinated)
as
(
SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location ORDER BY dea.location ,dea.Date) AS RollingPeopleVacc

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
)

SELECT *, (RollingPeopleVaccinated/Population)*100
FROM PopVsVac


---TEMP TABLE
DROP TABLE IF EXISTS #PercentPopVac
CREATE TABLE #PercentPopVac

( Continent nvarchar(255),
Location nvarchar(255),
Date Datetime,
Population numeric,
New_Vaccinations numeric,
RollingPeopleVaccinated numeric
)

INSERT INTO #PercentPopVac

SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location ORDER BY dea.location ,dea.Date) AS RollingPeopleVacc

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
ORDER BY 2,3;

---How many ppl in a country are vaccinated?
WITH PopVsVac(Continent,Location,Date,Population, New_vaccinations,RollingPeopleVaccinated)
as
(
SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location ORDER BY dea.location ,dea.Date) AS RollingPeopleVacc

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null
)

SELECT *, (RollingPeopleVaccinated/Population)*100
FROM #PercentPopVac;


---CREATE VIEW TO STORE DATA FOR LATER VISUALIZATION

CREATE VIEW PercentPopVacc AS
SELECT dea.continent, dea.location,dea.date,dea.population, vac.new_vaccinations,
SUM(CAST(vac.new_vaccinations AS INT)) OVER (PARTITION BY dea.Location ORDER BY dea.location ,dea.Date) AS RollingPeopleVacc

FROM PortfolioProject..CovidDeaths$ dea
JOIN PortfolioProject..CovidVacc vac
	ON
	dea.location=vac.location
	AND
	dea.date=vac.date
WHERE dea.continent is not null;

SELECT *
FROM PercentPopVacc



