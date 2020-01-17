// ReSharper disable InconsistentNaming
// ReSharper disable PrivateFieldCanBeConvertedToLocalVariable

using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using LiteDB;

[assembly: InternalsVisibleTo("OilGas.Data.RRC.Texas")]
[assembly: InternalsVisibleTo("OilGas.Data.FracFocus")]

namespace OilGas.Data
{
    public sealed class PersistentData
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        static PersistentData()
        {
        }

        private static readonly PersistentData _instance = new PersistentData();

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static PersistentData GetInstance()
        {
            return _instance;
        }

        //static char[] specialChars = new char[]
        //{
        //    '/', '\\', '"', ':', '|', '<', '>', '?', '*'
        //};

        internal DataBase DataBase
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        private PersistentData()
        {
        }

        internal static PersistentData Instance
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return GetInstance(); }
        }

        private static bool IsInitializeCollection_TCollection = false;

        public static void Initialize(DataStorage dbPath)
        {
            if(dbPath == null)
            {
                throw new NullReferenceException(nameof(dbPath));
            }

            if(Instance.DataBase == null)
            {
                Instance.DataBase = new DataBase(dbPath);
            }
            else if(Instance.DataBase.DatabasePath != dbPath.FullPath)
            {
                Instance.DataBase.Dispose();

                Instance.DataBase = new DataBase(dbPath);
            }
        }

        public static void InitializeCollection<TCollection>()
            where TCollection : class, IDataTable
        {
            if(!IsInitializeCollection_TCollection)
            {
                Instance.DataBase.Initialize<TCollection>();
                IsInitializeCollection_TCollection = true;
            }
        }

        private static bool IsInitializeCollection_TCollection_TInclude = false;

        public static void InitializeCollection<TCollection, TInclude>(Expression<Func<TCollection, TInclude>> includeExpr)
            where TCollection : class, IDataTable where TInclude : class
        {
            if(!IsInitializeCollection_TCollection_TInclude)
            {
                Instance.DataBase.Initialize<TCollection, TInclude>(includeExpr);
                IsInitializeCollection_TCollection_TInclude = true;
            }
        }

        #region TCollection, TInclude

        public static async Task<bool> Contains<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Contains(item);
        }

        public static async Task<bool> Contains<TCollection>(int id)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Contains<TCollection>(id);
        }

        public static async Task<bool> Contains<TCollection>(BsonExpression expression)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Contains<TCollection>(expression);
        }

        public static async Task<BsonValue> Add<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Add(item);
        }

        public static async Task AddRange<TCollection>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable
        {
            await Instance.DataBase.AddRange(items);
        }

        public static async Task<bool> Delete<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Delete(item);
        }

        public static async Task<bool> Update<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.Update(item);
        }

        public static async Task<IEnumerable<TCollection>> FindBy<TCollection>(Query query)
            where TCollection : class, IDataTable
        {
            return await Instance.DataBase.FindBy<TCollection>(query);
        }

        public static IEnumerable<TCollection> FindBy<TCollection>(BsonExpression expression)
            where TCollection : class, IDataTable
        {
            return Instance.DataBase.FindBy<TCollection>(expression);
        }

        #endregion

        #region TCollection, TInclude

        public static async Task<bool> Contains<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Contains(item);
        }

        public static async Task<bool> Contains<TCollection, TInclude>(int id)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Contains<TCollection, TInclude>(id);
        }

        public static async Task<bool> Contains<TCollection, TInclude>(BsonExpression expression)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Contains<TCollection, TInclude>(expression);
        }

        public static async Task<BsonValue> Add<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Add(item);
        }

        public static async Task AddRange<TCollection, TInclude>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable where TInclude : class
        {
            await Instance.DataBase.AddRange(items);
        }

        public static async Task<bool> Delete<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Delete(item);
        }

        public static async Task<bool> Update<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.Update(item);
        }

        public static async Task<IEnumerable<TCollection>> FindBy<TCollection, TInclude>(Query query)
            where TCollection : class, IDataTable where TInclude : class
        {
            return await Instance.DataBase.FindBy<TCollection, TInclude>(query);
        }

        public static IEnumerable<TCollection> FindBy<TCollection, TInclude>(BsonExpression expression)
            where TCollection : class, IDataTable where TInclude : class
        {
            return Instance.DataBase.FindBy<TCollection, TInclude>(expression);
        }

        public static IEnumerable<TCollection> FindBy<TCollection, TInclude>(Expression<Func<TCollection, bool>> expression)
            where TCollection : class, IDataTable where TInclude : class
        {
            return Instance.DataBase.FindBy<TCollection, TInclude>(expression);
        }

        #endregion
    }
}