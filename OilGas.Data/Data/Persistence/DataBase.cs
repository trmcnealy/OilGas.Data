using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq.Expressions;
using System.Threading.Tasks;

using LiteDB;

namespace OilGas.Data
{
    internal class DataBase : IDisposable
    {
        public string DatabasePath { get; }

        private LiteDatabase Database { get; }

        public DataBase(DataStorage databasePath)
        {
            DatabasePath = databasePath.FullPath;
            Database     = new LiteDatabase(DatabasePath);
        }

        ~DataBase()
        {
            Dispose(false);
        }

        private void Dispose(bool disposing)
        {
            if(disposing)
            {
                Database?.Dispose();
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        #region TCollection

        public void Initialize<TCollection>()
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            collection.EnsureIndex(x => x.Id,
                                   true);
        }

        public LiteCollection<TCollection> GetCollection<TCollection>(LiteDatabase database)
            where TCollection : class, IDataTable
        {
            return database.GetCollection<TCollection>(typeof(TCollection).Name);
        }

        public async Task<bool> Contains<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Exists(x => x == item));
        }

        public async Task<bool> Contains<TCollection>(int id)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Exists(x => x.Id == id));
        }

        public async Task<bool> Contains<TCollection>(BsonExpression expression)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Exists(expression));
        }

        public async Task<BsonValue> Add<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Insert(item));
        }

        public async Task AddRange<TCollection>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            await Task.Run(() =>
                           {
                               foreach(TCollection item in items)
                               {
                                   if(collection.Exists(x => x == item))
                                   {
                                       TCollection existingItem = collection.FindOne(x => x.Id == item.Id);

                                       //existingItem.AddRange(item.DocumentRefs);

                                       collection.Update(existingItem);
                                   }
                                   else
                                   {
                                       collection.Insert(item);
                                   }
                               }
                           });
        }

        public async Task<bool> Delete<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Delete(item.Id));
        }

        public async Task<bool> Update<TCollection>(TCollection item)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Update(item.Id,
                                                          item));
        }

        public async Task<IEnumerable<TCollection>> FindBy<TCollection>(Query query)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return await Task.Run(() => collection.Find(query));
        }

        public IEnumerable<TCollection> FindBy<TCollection>(BsonExpression predicate)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return collection.Find(predicate);
        }

        public IEnumerable<TCollection> FindBy<TCollection>(Expression<Func<TCollection, bool>> predicate)
            where TCollection : class, IDataTable
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database);

            return collection.Find(predicate);
        }

        #endregion

        #region TCollection, TInclude

        private static readonly ConcurrentDictionary<Type, Expression> includeExpressions = new ConcurrentDictionary<Type, Expression>();

        private static Expression GetIncludeExpression<TInclude>()
            where TInclude : class
        {
            if(!includeExpressions.TryGetValue(typeof(TInclude),
                                                out Expression expression))
            {
                throw new Exception("Must call Initialize method first.");
            }

            return expression;
        }

        public void Initialize<TCollection, TInclude>(Expression<Func<TCollection, TInclude>> includeExpr)
            where TCollection : class, IDataTable where TInclude : class
        {
            if(!includeExpressions.ContainsKey(typeof(TInclude)))
            {
                includeExpressions.GetOrAdd(typeof(TInclude),
                                             includeExpr);
            }

            LiteCollection<TCollection> collection = GetCollection<TCollection>(Database).Include(includeExpr);

            collection.EnsureIndex(x => x.Id,
                                   true);
        }

        public LiteCollection<TCollection> GetCollection<TCollection, TInclude>(LiteDatabase database)
            where TCollection : class, IDataTable where TInclude : class
        {
            return database.GetCollection<TCollection>(typeof(TCollection).Name).Include((Expression<Func<TCollection, TInclude>>)GetIncludeExpression<TInclude>());
        }

        public async Task<bool> Contains<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Exists(x => x == item));
        }

        public async Task<bool> Contains<TCollection, TInclude>(int id)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Exists(x => x.Id == id));
        }

        public async Task<bool> Contains<TCollection, TInclude>(BsonExpression expression)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Exists(expression));
        }

        public async Task<BsonValue> Add<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Insert(item));
        }

        public async Task AddRange<TCollection, TInclude>(IEnumerable<TCollection> items)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            await Task.Run(() =>
                           {
                               foreach(TCollection item in items)
                               {
                                   if(collection.Exists(x => x == item))
                                   {
                                       TCollection existingItem = collection.FindOne(x => x.Id == item.Id);

                                       //existingItem.AddRange(item.DocumentRefs);

                                       collection.Update(existingItem);
                                   }
                                   else
                                   {
                                       collection.Insert(item);
                                   }
                               }
                           });
        }

        public async Task<bool> Delete<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Delete(item.Id));
        }

        public async Task<bool> Update<TCollection, TInclude>(TCollection item)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Update(item.Id,
                                                          item));
        }

        public async Task<IEnumerable<TCollection>> FindBy<TCollection, TInclude>(Query query)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return await Task.Run(() => collection.Find(query));
        }

        public IEnumerable<TCollection> FindBy<TCollection, TInclude>(BsonExpression predicate)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return collection.Find(predicate);
        }

        public IEnumerable<TCollection> FindBy<TCollection, TInclude>(Expression<Func<TCollection, bool>> predicate)
            where TCollection : class, IDataTable where TInclude : class
        {
            LiteCollection<TCollection> collection = GetCollection<TCollection, TInclude>(Database);

            return collection.Find(predicate);
        }

        #endregion

        //public void AddRange(SortedDictionary<string, T> items, string tableName)
        //{
        //        LiteCollection<T> words = Instance.GetCollection<T>(WordsCollectionName);
        //
        //        //Parallel.ForEach(lines, (OcrLine ocrLine) =>
        //        foreach (T existingWord in words.FindAll())
        //        {
        //            if (items.ContainsKey(existingWord.Text))
        //            {
        //                T existingItem = items[existingWord.Text];
        //
        //                existingItem.AddRange(existingWord.DocumentRefs);
        //
        //                items[existingWord.Text] = existingItem;
        //            }
        //            else
        //            {
        //                items.Add(existingWord.Text, existingWord);
        //            }
        //        }
        //
        //        int index = 0;
        //
        //        using (var iter = items.GetEnumerator())
        //        {
        //            T currentItem;
        //
        //            while (iter.MoveNext())
        //            {
        //                currentItem = iter.Current.Value;
        //
        //                if (words.Exists(x => x.Text == currentItem.Text))
        //                {
        //                    T existingItem = words.Include(x => x.DocumentRefs).FindOne(entry => entry.Text == currentItem.Text);
        //
        //                    existingItem.AddRange(currentItem.DocumentRefs);
        //
        //                    words.Update(existingItem);
        //                    //await Task.Run(() => words.Update(existingItem));
        //                }
        //                else if (words.FindById(index) != null)
        //                {
        //                    currentItem.Id = index++;
        //                    words.Update(currentItem.Id, currentItem);
        //                }
        //                else
        //                {
        //                    currentItem.Id = index++;
        //                    words.Insert(currentItem);
        //                }
        //            }
        //        }
        //
        //}

        //public async Task<IEnumerable<T>> FindByText(string text)
        //{
        //    throw new NotImplementedException();
        //    //using (LiteDatabase Instance = new LiteDatabase(typeof(T).Name))
        //    //{
        //    //    LiteCollection<T> documents = Instance.GetCollection<T>(DocumentsCollectionName);

        //    //    IEnumerable<T> existingItems = documents.Include(x => x.PdfWords).Find(item => item.Text == text);

        //    //    return existingItems;
        //    //}
        //}
    }
}