// ReSharper disable InconsistentNaming
// ReSharper disable PrivateFieldCanBeConvertedToLocalVariable

using System;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

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

        internal Database Database
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get;
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            set;
        }

        private PersistentData()
        {
        }

        ~PersistentData()
        {
            Instance.Database.Dispose();
        }

        internal static PersistentData Instance
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            get { return GetInstance(); }
        }

        private static bool IsInitializeCollection_TCollection = false;
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Initialize(DataStorage dbPath)
        {
            if(dbPath == null)
            {
                throw new NullReferenceException(nameof(dbPath));
            }

            if(Instance.Database == null)
            {
                Instance.Database = new Database(dbPath);
            }
            else if(Instance.Database.DatabasePath != dbPath.FullPath)
            {
                Instance.Database.Dispose();

                Instance.Database = new Database(dbPath);
            }
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void InitializeCollection<TCollection>()
            where TCollection : class, IDataTable<Guid>
        {
            if(!IsInitializeCollection_TCollection)
            {
                Instance.Database.Initialize<TCollection>();
                IsInitializeCollection_TCollection = true;
            }
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void Commit()
        {
            Instance.Database.Commit();
        }

        //private static bool IsInitializeCollection_TCollection_TInclude = false;

        //public static void InitializeCollection<TCollection, TInclude>(Expression<Func<TCollection, TInclude>> includeExpr)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    if(!IsInitializeCollection_TCollection_TInclude)
        //    {
        //        Instance.Database.Initialize<TCollection, TInclude>(includeExpr);
        //        IsInitializeCollection_TCollection_TInclude = true;
        //    }
        //}

        #region TCollection
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> Contains<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.Contains(item);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> Contains<TCollection>(Guid id)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.Contains<TCollection>(id);
        }

        //public static async Task<bool> Contains<TCollection>(Expression expression)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return await Instance.Database.Contains<TCollection>(expression);
        //}
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> Add<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.Add(item);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task AddRange<TCollection>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable<Guid>
        {
            await Instance.Database.AddRange(items);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<bool> Delete<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.Delete(item);
        }

        //public static async Task<bool> Update<TCollection>(TCollection item)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return await Instance.Database.Update(item);
        //}

        //public static async Task<IEnumerable<TCollection>> FindBy<TCollection>(Query query)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return await Instance.Database.FindBy<TCollection>(query);
        //}
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<TCollection> FindBy<TCollection>(Guid key)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.FindBy<TCollection>(key);
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<TCollection> FindBy<TCollection>(Func<TCollection, bool> expression)
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.FindBy<TCollection>(expression);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static async Task<IEnumerable<KeyValuePair<Guid, TCollection>>> FindAll<TCollection>()
            where TCollection : class, IDataTable<Guid>
        {
            return await Instance.Database.FindAll<TCollection>();
        }

        #endregion

        #region TCollection, TInclude

        //public static async Task<bool> Contains<TCollection, TInclude>(TCollection item)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Contains(item);
        //}

        //public static async Task<bool> Contains<TCollection, TInclude>(int id)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Contains<TCollection, TInclude>(id);
        //}

        //public static async Task<bool> Contains<TCollection, TInclude>(Expression expression)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Contains<TCollection, TInclude>(expression);
        //}

        //public static async Task<BsonValue> Add<TCollection, TInclude>(TCollection item)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Add(item);
        //}

        //public static async Task AddRange<TCollection, TInclude>(IEnumerable<TCollection> items)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    await Instance.Database.AddRange(items);
        //}

        //public static async Task<bool> Delete<TCollection, TInclude>(TCollection item)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Delete(item);
        //}

        //public static async Task<bool> Update<TCollection, TInclude>(TCollection item)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return await Instance.Database.Update(item);
        //}

        ////public static async Task<IEnumerable<TCollection>> FindBy<TCollection, TInclude>(Query query)
        ////    where TCollection : class, IDataTable<Guid> where TInclude : class
        ////{
        ////    return await Instance.Database.FindBy<TCollection, TInclude>(query);
        ////}

        //public static IEnumerable<TCollection> FindBy<TCollection, TInclude>(Expression expression)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return Instance.Database.FindBy<TCollection, TInclude>(expression);
        //}

        //public static IEnumerable<TCollection> FindBy<TCollection, TInclude>(Expression<Func<TCollection, bool>> expression)
        //    where TCollection : class, IDataTable<Guid> where TInclude : class
        //{
        //    return Instance.Database.FindBy<TCollection, TInclude>(expression);
        //}

        #endregion
    }
}