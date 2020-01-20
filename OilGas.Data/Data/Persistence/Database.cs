using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using System.Threading.Tasks;

namespace OilGas.Data
{
    public class Database : IDisposable
    {

        private readonly SemaphoreSlim _semaphoreSlim = new SemaphoreSlim(1,
                                                                          1);

        private readonly Dictionary<string, SerializableConcurrentDictionary<Guid, object>> _data =
            new Dictionary<string, SerializableConcurrentDictionary<Guid, object>>();

        public string DatabasePath { get; }

        public Database(DataStorage databasePath)
        {
            DatabasePath = databasePath.FullPath;

            if(File.Exists(DatabasePath))
            {
                using(FileStream fs = new FileStream(DatabasePath,
                                                     FileMode.Open))
                {
                    if(fs.Length != 0)
                    {
                        BinaryFormatter formatter = new BinaryFormatter();

                        _data = (Dictionary<string, SerializableConcurrentDictionary<Guid, object>>)formatter.Deserialize(fs);
                    }
                }
            }
        }

        #region Dispose

        ~Database()
        {
            Dispose(false);
        }

        private void Dispose(bool disposing)
        {
            if(disposing)
            {
                Commit();
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        #endregion

        public void Commit()
        {
            using(FileStream fs = new FileStream(DatabasePath,
                                                 FileMode.OpenOrCreate))
            {
                BinaryFormatter formatter = new BinaryFormatter();

                formatter.Serialize(fs,
                                    _data);
            }
        }

        #region Call Methods

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private async Task<TReturn> CallSynchronizedAsync<TCollection, TReturn>(Func<SerializableConcurrentDictionary<Guid, object>, Guid, Task<TReturn>> func,
                                                                                Guid                                                                      key)
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await func(_data[typeof(TCollection).Name],
                                                             key));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private async Task<TReturn> CallSynchronizedAsync<TCollection, TReturn>(Func<SerializableConcurrentDictionary<Guid, object>, Guid, object, Task<TReturn>> func,
                                                                                Guid                                                                              key,
                                                                                object                                                                            value)
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await func(_data[typeof(TCollection).Name],
                                                             key,
                                                             value));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private async Task<TReturn> CallSynchronizedAsync<TCollection, TReturn>(
            Func<SerializableConcurrentDictionary<Guid, object>, Guid, object, Func<Guid, object, object>, Task<TReturn>> func,
            Guid                                                                                                          key,
            object                                                                                                        value,
            Func<Guid, object, object>                                                                                    updateValueFactory)
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await func(_data[typeof(TCollection).Name],
                                                             key,
                                                             value,
                                                             updateValueFactory));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private async Task<TReturn> CallSynchronizedAsync<TCollection, TReturn>(Func<SerializableConcurrentDictionary<Guid, object>, Guid, Func<Guid, object>, Task<TReturn>> func,
                                                                                Guid                                                                                          key,
                                                                                object                                                                                        value,
                                                                                Func<Guid, object>                                                                            addValueFactory)
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await func(_data[typeof(TCollection).Name],
                                                             key,
                                                             addValueFactory));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private async Task<TReturn> CallSynchronizedAsync<TCollection, TReturn>(Func<SerializableConcurrentDictionary<Guid, object>, KeyValuePair<Guid, object>, Task<TReturn>> func,
                                                                                KeyValuePair<Guid, object>                                                                      keyValuePair)
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await func(_data[typeof(TCollection).Name],
                                                             keyValuePair));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        private static readonly Func<SerializableConcurrentDictionary<Guid, object>, Guid, Task<bool>> getContainsFunc = (dictionary,
                                                                                                                          key) => Task.FromResult(dictionary.ContainsKey(key));

        private static readonly Func<SerializableConcurrentDictionary<Guid, object>, Guid, object, Task<bool>> getAddFunc = (dictionary,
                                                                                                                             key,
                                                                                                                             value) => Task.FromResult(dictionary.TryAdd(key,
                                                                                                                                                                         value));

        private static readonly Func<SerializableConcurrentDictionary<Guid, object>, Guid, Task<bool>> getRemoveFunc = (dictionary,
                                                                                                                        key) => Task.FromResult(dictionary.TryRemove(key,
                                                                                                                                                                     out _));

        private static readonly Func<SerializableConcurrentDictionary<Guid, object>, Guid, object, Func<Guid, object, object>, Task<object>> getAddOrUpdateFunc = (dictionary,
                                                                                                                                                                   key,
                                                                                                                                                                   addValue,
                                                                                                                                                                   updateValueFactory) =>
                                                                                                                                                                      Task.
                                                                                                                                                                          FromResult(dictionary.
                                                                                                                                                                                         AddOrUpdate(key,
                                                                                                                                                                                                     addValue,
                                                                                                                                                                                                     updateValueFactory));

        #endregion

        public void Initialize<TCollection>()
            where TCollection : class, IDataTable<Guid>
        {
            if(!_data.TryGetValue(typeof(TCollection).Name,
                                  out _))
            {
                _data.TryAdd(typeof(TCollection).Name,
                             new SerializableConcurrentDictionary<Guid, object>());
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<bool> Contains<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return CallSynchronizedAsync<TCollection, bool>(getContainsFunc,
                                                            item.Key);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<bool> Contains<TCollection>(Guid key)
            where TCollection : class, IDataTable<Guid>
        {
            return CallSynchronizedAsync<TCollection, bool>(getContainsFunc,
                                                            key);
        }

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public Task<bool> Contains<TCollection>(Expression expression)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return await Instance.DataBase.Contains<TCollection>(expression);
        //}

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<bool> Add<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return CallSynchronizedAsync<TCollection, bool>(getAddFunc,
                                                            item.Key,
                                                            item);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<object> AddOrUpdate<TCollection>(TCollection                item,
                                                     Func<Guid, object, object> updateValueFactory)
            where TCollection : class, IDataTable<Guid>
        {
            return CallSynchronizedAsync<TCollection, object>(getAddOrUpdateFunc,
                                                              item.Key,
                                                              item,
                                                              updateValueFactory);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task AddRange<TCollection>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable<Guid>
        {
            foreach(TCollection item in items)
            {
                await Add(item);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public Task<bool> Delete<TCollection>(TCollection item)
            where TCollection : class, IDataTable<Guid>
        {
            return CallSynchronizedAsync<TCollection, bool>(getRemoveFunc,
                                                            item.Key);
        }

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public async Task<bool> Update<TCollection>(TCollection item)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return await Instance.DataBase.Update(item);
        //}

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<TCollection> FindBy<TCollection>(Guid key)
            where TCollection : class, IDataTable<Guid>
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(() =>
                                      {
                                          if(_data[typeof(TCollection).Name].TryGetValue(key,
                                                                                         out object value))
                                          {
                                              return (TCollection)value;
                                          }

                                          return null;
                                      });
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<TCollection> FindBy<TCollection>(Func<TCollection, bool> expression)
            where TCollection : class, IDataTable<Guid>
        {
            try
            {
                await _semaphoreSlim.WaitAsync();

                return await Task.Run(async () => await Task.FromResult(_data[typeof(TCollection).Name].Values.AsParallel().Cast<TCollection>().FirstOrDefault(expression)));
            }
            finally
            {
                _semaphoreSlim.Release();
            }
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public async Task<IEnumerable<KeyValuePair<Guid, TCollection>>> FindAll<TCollection>()
        {
            return await Task.FromResult(_data[typeof(TCollection).Name].AsParallel().Cast<TCollection>());
        }

        //[MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        //public IEnumerable<TCollection> FindBy<TCollection>(Expression expression)
        //    where TCollection : class, IDataTable<Guid>
        //{
        //    return Instance.DataBase.FindBy<TCollection>(expression);
        //}
    }

    public static class Enumerable
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static IEnumerable<KeyValuePair<Guid, TResult>> Cast<TResult>(this IEnumerable<KeyValuePair<Guid, object>> source)
        {
            if (source == null)
            {
                throw new ArgumentNullException(nameof(source));
            }
            
            return CastIterator<TResult>(source);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static IEnumerable<KeyValuePair<Guid, TResult>> CastIterator<TResult>(IEnumerable<KeyValuePair<Guid, object>> source)
        {
            foreach (KeyValuePair<Guid, object> obj in source)
            {
                yield return new KeyValuePair<Guid, TResult>(obj.Key, (TResult)obj.Value);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static KeyValuePair<Guid, TResult> Cast<TResult>(this KeyValuePair<Guid, object> obj)
        {
            return new KeyValuePair<Guid, TResult>(obj.Key, (TResult)obj.Value);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static IEnumerable<TResult> Cast<TResult>(this IEnumerable<object> source)
        {
            if (source == null)
            {
                throw new ArgumentNullException(nameof(source));
            }
            
            return CastIterator<TResult>(source);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        private static IEnumerable<TResult> CastIterator<TResult>(IEnumerable<object> source)
        {
            foreach (object obj in source)
            {
                yield return (TResult)obj;
            }
        }
    }
}