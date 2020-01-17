using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using LiteDB;

namespace OilGas.Data.FracFocus
{
    public abstract class FracFocusDataAdapter
    {
        private FracFocusDataAdapter()
        {
        }

        public static async void RegistryUploadCsvToDb(string      filePath,
                                                       DataStorage dbPath = default(DataStorage))
        {
            string fileContents = File.ReadAllText(filePath);

            CsvReader csvReader = new CsvReader(fileContents);

            List<string[]> data = csvReader.ReadFile(1);

            List<Registry> registryUploadList = new List<Registry>(data.Count);

            foreach(string[] entry in data)
            {
                //pKey	JobStartDate	JobEndDate	APINumber	StateNumber	CountyNumber	OperatorName	WellName	Latitude	Longitude	Projection	TVD	TotalBaseWaterVolume	TotalBaseNonWaterVolume	StateName	CountyName	FFVersion	FederalWell	IndianWell

                registryUploadList.Add(new Registry(entry));
            }

            PersistentData.Initialize(dbPath ?? new DataStorage());

            PersistentData.InitializeCollection<Registry>();
            //PersistentData.InitializeCollection<WellProduction, List<WellProductionRecord>>(x => x.Records);

            await PersistentData.AddRange(registryUploadList);

            //await PersistentData.Add<WellProduction, List<WellProductionRecord>>(wellProduction);
        }

        public static async void FracFocusRegistryCsvToDb(string      filePath,
                                                          DataStorage dbPath = default(DataStorage))
        {
            string fileContents = File.ReadAllText(filePath);

            CsvReader csvReader = new CsvReader(fileContents);

            List<string[]> data = csvReader.ReadFile(1);

            List<Registry> registryUploadList = new List<Registry>(data.Count);

            List<PurposeTable> purposeTableList = new List<PurposeTable>(data.Count);

            List<Ingredients> ingredientsList = new List<Ingredients>(data.Count);

            Registry     registry;
            PurposeTable purposeTable;

            foreach(string[] entry in data)
            {
                registry = new Registry(entry.Take(20).ToArray());
                registryUploadList.Add(registry);

                BsonExpression registryKey = Query.EQ("Key",
                                                      registry.Key.ToString());

                if(await PersistentData.Contains<Registry>(registryKey))
                {
                    registry = PersistentData.FindBy<Registry>(registryKey).First();
                }

                purposeTable = new PurposeTable(registry,
                                                entry.Skip(21).Take(8).ToArray());

                purposeTableList.Add(purposeTable);

                ingredientsList.Add(new Ingredients(purposeTable,
                                                    entry.Skip(29).ToArray()));
            }

            PersistentData.Initialize(dbPath ?? new DataStorage());

            PersistentData.InitializeCollection<PurposeTable, Registry>(x => x.Registry);
            PersistentData.InitializeCollection<Ingredients, PurposeTable>(x => x.PurposeTable);
            //PersistentData.InitializeCollection<WellProduction, List<WellProductionRecord>>(x => x.Records);

            await PersistentData.AddRange(registryUploadList);

            await PersistentData.AddRange<PurposeTable, Registry>(purposeTableList);

            await PersistentData.AddRange<Ingredients, PurposeTable>(ingredientsList);
        }
    }
}