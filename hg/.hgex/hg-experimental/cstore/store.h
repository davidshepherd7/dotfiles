// store.h - c++ declarations for a data store
//
// Copyright 2017 Facebook, Inc.
//
// This software may be used and distributed according to the terms of the
// GNU General Public License version 2 or any later version.
//
// no-check-code
//
#ifndef STORE_H
#define STORE_H

#include "key.h"
#include <memory>
#include <cstddef>

class ConstantStringRef {
  private:
    std::shared_ptr<std::string> str_;
  public:
    ConstantStringRef() = default;

    /** Make a copy of the provided string buffer */
    ConstantStringRef(const char *str, size_t size)
        : str_(std::make_shared<std::string>(str, size)) {}

    /** Take ownership of an existing string */
    ConstantStringRef(std::string &&str)
        : str_(std::make_shared<std::string>(std::move(str))) {}

    /** Take ownership of an existing shared_ptr<string> */
    ConstantStringRef(std::shared_ptr<std::string> str) : str_(str) {}

    const char *content() {
      if (str_) {
        return str_->data();
      }
      return NULL;
    }

    size_t size() {
      if (str_) {
        return str_->size();
      }
      return 0;
    }
};

class Store {
  public:
    virtual ~Store() {}
    virtual ConstantStringRef get(const Key &key) = 0;
};

#endif //STORE_H
