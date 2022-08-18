use std::mem;
use std::fmt::{Debug, Formatter};
use std::iter::Empty;
use crate::object::ObjString;
use crate::{As, Value};

const TABLE_MAX_LOAD: f32 = 0.75;
pub struct Table {
    count : usize,
    capacity : usize,
    entries : Vec<Entry>
}

#[derive(Debug,Clone)]
pub struct Entry {
    key : ObjString,
    value: Value
}

impl Entry {
    fn empty() -> Self {
        Self {
            key: ObjString::empty(),
            value: Value::empty()
        }

    }

    pub fn free(&mut self) {
        println!("freeing entry {:?}",&self.key.as_str());
        self.key.free();
        self.value.free();
    }
}

impl Drop for Entry {
    fn drop(&mut self) {
        self.key.free();
        self.value.free();
    }
}
impl Debug for Table {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for entry in &self.entries {
            string.push_str(&format!("{:?}",entry));
            string.push_str("\n");
        }

        f.write_str(&string)
    }
}
impl Table {
    pub fn new() -> Self {
        Self {
            count:0 ,
            capacity:0,
            entries: vec![]
        }
    }

    pub fn free(&mut self) {
        self.capacity = 0;
        self.count = 0;
        for entry in self.entries.iter_mut(){
            entry.free()
        }
        mem::replace(&mut self.entries, vec![]);
    }

    pub fn set(&mut self, key : ObjString, value : Value) -> bool {

        /**
          if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
          int capacity = GROW_CAPACITY(table->capacity);
          adjustCapacity(table, capacity);
        }
        */
        /**
           Entry* entry = findEntry(table->entries, table->capacity, key);
        bool isNewKey = entry->key == NULL;
        if (isNewKey) table->count++;

        entry->key = key;
        entry->value = value;
        return isNewKey;
         */

        if ((self.count + 1) as f32 > (self.capacity as f32) * TABLE_MAX_LOAD) {
            // we need to grow our array to
            let capacity = self.grow_capacity();
            // println!("before adjusting capacity : \n{:?}",&self);
            // println!("*********************************************************");
            self.adjust_capacity(capacity);
           // println!("after adjusting capacity : \n{:?}",&self);
        }

        let mut entry = Self::find_entry_mut(&mut self.entries ,self.capacity, &key);
        let isNewKey = match (*entry).value.rep {
            As::Empty => true,
            _ => false
        };
        (*entry).key = key;
        (*entry).value = value;


        if (isNewKey && entry.value.is_empty()) {
            // only increment count, if this was a truly empty slot
            // tombstones were once valid entries and were counted, so no need to count them again
            self.count += 1;
        }
        isNewKey


    }

    fn grow_capacity(&self) -> usize {
        let updated_capacity = if(self.capacity <8){ 8 } else {self.capacity * 2};
        // self.entries.resize_with(updated_capacity, || Entry::empty());
        // self.capacity = updated_capacity;
        updated_capacity
    }

    fn adjust_capacity(&mut self,capacity : usize) {
        /**
         Entry* entries = ALLOCATE(Entry, capacity);
        for (int i = 0; i < capacity; i++) {
          entries[i].key = NULL;
          entries[i].value = NIL_VAL;
        }

        table->entries = entries;
        table->capacity = capacity;
         */


        /**
         for (int i = 0; i < table->capacity; i++) {
          Entry* entry = &table->entries[i];
          if (entry->key == NULL) continue;

          Entry* dest = findEntry(entries, capacity, entry->key);
          dest->key = entry->key;
          dest->value = entry->value;
        }
         */

        // We want to move from a smaller to a bigger hash map

        // first we create the bigger hash map with empty values
        let mut updated_entries = vec![Entry::empty(); capacity]; // we have resized the array of entries

        // now for every entry in the previous map (smaller map)
        // if the entry is empty, then there's nothing to do, as the destination (bigger) map is all filled with empty values
        // if the entry is not empty, then we find the entry/bucket the key is supposed to be in the bigger map.
        // this is important because due to collision, an entry could be elsewhere
        /*
         As an example, if the initial map size was 8, and we want to store an entry with a hash value of 22,
        it will be in bucket 6 (22 % 8), now if we want to add another entry with hash of 30, its bucket is 6 also and that causes a collision,
        which 'may' result in the entry going to bucket 7 instead.

        Now, when we create a bigger map (16 buckets), there's more space and as such, now if we find the buckets for hash 22 and 30, they don't collide
        anymore, they go into buckets 6 and 14 respectively.
        That's why when copying over elements from the smaller map to the bigger map, we need to find the appropriate bucket/ entry for each element from the smaller map
         */

        self.count = 0;
        for entry in &self.entries {
           if(entry.key.is_empty()) {
               // nothing to do
               continue
           } else {
               // find the destination in the new/updated/bigger map and store the entry from the smaller map there instead
               let destination = Self::find_entry_mut(&mut updated_entries,capacity,&entry.key);
               destination.key = entry.key;
               destination.value = entry.value;
               self.count += 1

           }
        }

        mem::replace(&mut self.entries, updated_entries );
        self.capacity = capacity;
    }


    pub fn remove(&mut self, key : &ObjString) -> bool {
        if(self.count == 0) {
            false
        } else {
            let entry = Table::find_entry_mut(&mut self.entries,self.capacity, &key);
            if(entry.key.is_empty()){
                // this is an empty entry, not a corresponding key , no need to tombstone it
                false
            } else {
                entry.key = ObjString::empty();
                entry.value = Value::nil_value();
                true
            }
        }
    }

    pub fn find_entry_mut<'a>(entries: &'a mut Vec<Entry>, capacity: usize, key: &ObjString) -> &'a mut Entry {
        /**
         static Entry* findEntry(Entry* entries, int capacity,
                        ObjString* key) {
          uint32_t index = key->hash % capacity;
          for (;;) {
            Entry* entry = &entries[index];
            if (entry->key == key || entry->key == NULL) {
              return entry;
            }

            index = (index + 1) % capacity;
          }
        }
         */

        let mut index = (key.hash as usize % capacity) as usize;
        // let mut tombstone : &mut Entry = &mut Entry::empty();
        let mut tombstone_found = false;
        let mut tomb_stone_index: Option<usize> = None;
        loop {
            let mut entry = (entries.get(index).expect("index out of bounds"));
            match (*entry).key {
                p @ ObjString { .. } => {
                    if(p == *key ){
                        return entries.get_mut(index).expect("index out of bounds");
                    } else {
                        if p.is_empty() {
                            if entry.value.is_empty() {
                                // here if the value is empty, there's a probability we've found
                                // a previous tombstone so we should return that tombstone, or else
                                // this really (naturally) empty slot / entry
                                println!("index is {}",index);
                                if tomb_stone_index.is_none() {
                                    // if we've not found a tombstone yet, just return this actually empty slot as it was never used
                                    return entries.get_mut(index).expect("index out of bounds");
                                } else {
                                    // if we have an empty entry, still haven't found the exact key and tombstone exists,
                                    // return/reuse the tombstone
                                    return entries.get_mut(tomb_stone_index.unwrap()).expect("index out of bounds");
                                }
                                //return entries.get_mut(index).expect("index out of bounds");
                            } else if (entry.value.is_nil()){
                                // we found a tombstone, save the index
                                tomb_stone_index = Some(index);
                            }
                        }
                    }


                    // if (p == *key || (*entry).key.is_empty()) {
                    //     return entries.get_mut(index).expect("index out of bounds");
                    // }
                }
            }
            index = (index + 1) % capacity;
        }
    }
    // pub fn find_entry<'a>(entries : &'a mut Vec<Entry>, capacity : usize ,key : &ObjString) -> &'a Entry {
    //
    //     /**
    //      static Entry* findEntry(Entry* entries, int capacity,
    //                     ObjString* key) {
    //       uint32_t index = key->hash % capacity;
    //       for (;;) {
    //         Entry* entry = &entries[index];
    //         if (entry->key == key || entry->key == NULL) {
    //           return entry;
    //         }
    //
    //         index = (index + 1) % capacity;
    //       }
    //     }
    //      */
    //
    //     let mut index = (key.hash as usize % capacity) as usize;
    //     unsafe {
    //         loop {
    //             let mut entry  = (entries.get(index).expect("index out of bounds"));
    //             match (*entry).key {
    //                 p @ObjString { .. } =>  {
    //                     if(p == *key || (*entry).key.is_empty()) {
    //                         return entry;
    //                     }
    //                 }
    //             }
    //             index = (index + 1) % capacity;
    //         }
    //     }
    // }

    fn get(&mut self, object : &ObjString) -> Option<&Value> {
        /**
           if (table->count == 0) return false;

        Entry* entry = findEntry(table->entries, table->capacity, key);
        if (entry->key == NULL) return false;

        *value = entry->value;
        return true;
         */
        if(self.count == 0) {
            None
        }else {
            let entry = Table::find_entry_mut(&mut self.entries,self.capacity,&object);
            if(entry.key.is_empty()){
                None
            } else {
                Some(&entry.value)
            }
        }

    }

    fn add_all(from : &mut Table, to : &mut Table) {
        /**
        void tableAddAll(Table* from, Table* to) {
          for (int i = 0; i < from->capacity; i++) {
            Entry* entry = &from->entries[i];
            if (entry->key != NULL) {
              tableSet(to, entry->key, entry->value);
            }
          }
        }
        */

        for from_entry in &from.entries {
            if(!from_entry.key.is_empty()) {
                to.set(from_entry.key, from_entry.value);
            }
        }


    }
}


#[cfg(test)]
mod tests {
    use crate::object::Obj;
    use super::*;
    #[test]
    fn test_table(){

        let mut map = Table::new();
      //
      //   // these two collide
      //   map.set(ObjString::from_buffer("costarring".as_bytes()),Value::bool_value(true));
      // //  map.set(ObjString::from_buffer("liquid".as_bytes()),Value::bool_value(true));
      //
      //   // these collide
      //   map.set(ObjString::from_buffer("declinate".as_bytes()),Value::bool_value(true));
      //   map.set(ObjString::from_buffer("macallums".as_bytes()),Value::bool_value(true));
      //
      //
      //   let expected_value = Value::number_value(700f64);
      //   let zinke = ObjString::from_buffer("zinke".as_bytes());
      //   let zinke2 = zinke.clone();
      //
      //   println!("is equal : {}", {&zinke == &zinke2});
      //   //map.set(ObjString::from_buffer("altarage".as_bytes()),expected_value);
      //   map.set(zinke,expected_value.clone());
      //
      //   println!("{:?}",&map);
      //
      //   // let result = map.get(&ObjString::from_buffer("altarage".as_bytes()));
      //   // assert_eq!(result, Some(&expected_value));
      //
      //   let res = map.remove(&zinke2);
      //   println!("result is {}",res);
      //
      //   println!("{:?}",&map);
      //   map.set(ObjString::from_buffer("0".as_bytes()),Value::bool_value(true));
      //
      //   println!("{:?}",&map);

        map.set(ObjString::from_buffer("4".as_bytes()),Value::number_value(4_f64));
        map.set(ObjString::from_buffer("15".as_bytes()),Value::number_value(15_f64));
        map.set(ObjString::from_buffer("24".as_bytes()),Value::number_value(24_f64));
        println!("{:?}",&map);

        map.remove(&ObjString::from_buffer("15".as_bytes()));
        println!("{:?}",&map);

        let result = map.get(&ObjString::from_buffer("24".as_bytes()));
        println!("{:?}",result);
    }
}