#pragma once

#include <functional>
#include <algorithm>
#include <vector>
#include <type_traits>
#include <utility>

namespace ksignals
{
    template<int I> struct placeholder {};
} // ksignals::

namespace std {
    template<int I>
    struct is_placeholder< ::ksignals::placeholder<I>> : std::integral_constant<int, I> {};
} // std::

namespace ksignals {
    namespace detail {
    template<std::size_t... Is, class F, class... Args>
    auto easy_bind(std::integer_sequence<size_t, Is...>, F const& f, Args&&... args)
       {
           return std::bind(f, std::forward<Args>(args)..., placeholder<Is + 1>{}...);
       }
    } // detal::

    template<class R, class... FArgs, class... Args>
    auto easy_bind(std::function<R(FArgs...)> const& f, Args&&... args)
    {
       return detail::easy_bind(std::make_integer_sequence<size_t, sizeof...(FArgs)-sizeof...(Args)>{}, f, std::forward<Args>(args)...);
    }

    template<class R, typename T, class... FArgs, class... Args>
    auto easy_bind(R(T::*f)(FArgs...), T* meh, Args&&... args)
    {
       return detail::easy_bind(std::make_integer_sequence<size_t, sizeof...(FArgs)-sizeof...(Args)>{}, f, meh, std::forward<Args>(args)...);
    }

    template<class R, class... FArgs, class... Args>
    auto easy_bind(R(*f)(FArgs...), Args&&... args)
    {
       return detail::easy_bind(std::make_integer_sequence<size_t, sizeof...(FArgs)-sizeof...(Args)>{}, f, std::forward<Args>(args)...);
    }

    // We have to abstract member functions and non-member functions
    // Or do we not want non member functions
    class EventDelegateBase
    {
    public:
       virtual ~EventDelegateBase() = default;
    };

    template <typename... Args>
    class Event;

    template<typename... Args>
    class EventDelegate : public EventDelegateBase
    {
    private:
       std::vector<Event<Args...>*> v;

    public:
       EventDelegate(Event<Args...> &)
       {
           //e.connect(*this);
       }

      /* virtual ~EventDelegate()
       {
           for (auto e : v) {
                e->disconnect(*this);
           }

           v.clear();
       };*/

       void add(Event<Args...> *e)
       {
           v.push_back(e);
       }

       void remove(Event<Args...> *e)
       {
            if (v.empty()) {
               return;
            }
           
           v.erase(std::remove(std::begin(v), std::end(v), e), std::end(v));
       }


       virtual void invoke(Args &&...) = 0;
    };

    template<typename T, typename... Args>
    class EventDelegateMemberFunction : public EventDelegate<Args...>
    {
    private:
       T * t;
       void(T::*f)(Args...);
    public:
       EventDelegateMemberFunction(Event<Args...> &e, T *t, void(T::*f)(Args...))
           : EventDelegate<Args...>(e)
       {
           this->t = t;
           this->f = f;
       }

       virtual void invoke(Args&&...args) override
       {
           if (t) {
               (t->*f)(std::forward<Args>(args)...);
           }
       }
    };

    template<typename... Args>
    class EventDelegateFunctionObject : public EventDelegate<Args...>
    {
    private:
       std::function<void(Args...)> fn;

    public:
       EventDelegateFunctionObject(Event<Args...> &e, std::function<void(Args...)> f)
           : EventDelegate<Args...>(e),
           fn(f)
       { }

       virtual void invoke(Args&&... args) override
       {
           fn(std::forward<Args>(args)...);
       }
    };


    template<typename T, typename... Args>
    class EventDelegateFunctionPointer : public EventDelegate<Args...>
    {
       T * _function = nullptr;
    public:
       EventDelegateFunctionPointer(Event<Args...> &e, T * function)
           : EventDelegate<Args...>(e),
           _function(function)
       {

       }

       virtual void invoke(Args &&...args) override
       {
           if (_function) {
               (*_function)(std::forward<Args>(args)...);
           }
       }
    };

    template <typename... Args>
    class Event
    {
    private:
       std::vector<EventDelegate<Args...>*> things;

    public:

       Event() = default;
       ~Event()
       {
           for (auto i : things) {
               i->remove(this);
           }
       }

       Event& operator=(Event &&) = default;

       Event& operator = (const Event&) = delete;
       Event(const Event &) = delete;

       void operator()(Args... args)
       {
           // Loop through all connected things and call
           for (auto s : things) {
               s->invoke(std::forward<Args>(args)...);
           }
       }

       void connect(EventDelegate<Args...> &ed)
       {
           ed.add(this);
           things.push_back(&ed);
       }



       template <typename T, typename F, typename... FArgs>
       typename std::enable_if<std::is_class<T>::value, void>::type
           connect(T* t, F &&fn, FArgs&&... args)
       {
           // T is class
           connect(easy_bind(fn, t, std::forward<FArgs>(args)...));
       }

       template <typename T, typename F, typename... FArgs>
       typename std::enable_if<!std::is_class<T>::value, void>::type
           connect(T* t, F && arg0, FArgs&&... args)
       {
           connect(easy_bind(t, arg0, std::forward<FArgs>(args)...));
       }

       template <typename T, typename F>
       typename std::enable_if<!std::is_class<T>::value, void>::type
           connect(T* t)
       {
           auto f = new EventDelegateFunctionPointer<T, Args...>(*this, t);
           connect<Args...>(f);
       }

       template <typename T>
       void connect(T* t, void (T::*fn)(Args...))
       {
           auto f = new EventDelegateMemberFunction<T, Args...>(*this, t, fn);
           connect<Args...>(f);
       }

       template<typename F>
       void connect(F && fn)
       {
           // As this is either std::bind result
           // Or a lambda we can just pass it to the std::function overload
           //
           connect(std::function<void(Args...)>(std::forward<F>(fn)), *this);
       }

       template <typename T>
       struct identity
       {
           typedef T type;
       };

       //template<typename... Args>
       void connect(std::function<void(Args...)> fn, Event<Args...> & /* Trick to avoid recursion */)
       {
           auto f = new EventDelegateFunctionObject<Args...>(*this, fn);
           connect<Args...>(f);
       }

       //template<typename... Args>
       void connect(EventDelegate<Args...> *ed)
       {
           connect(*ed);
       }

       void disconnect(EventDelegate<Args...> &ed)
       {
           things.erase(std::remove(std::begin(things), std::end(things), &ed), std::end(things));
       }
    };

    class Delegate
    {
    public:
       ~Delegate()
       {
           for (auto d : v) {
               delete d;
           }

           v.clear();
       }

       template <typename T, typename E, typename F, typename... FArgs>
       typename std::enable_if<std::is_class<T>::value, void>::type
           connect(E &&e, T* t, F &&fn, FArgs&&... args)
       {
           // T is class
           connect(std::forward<E>(e), easy_bind(fn, t, std::forward<FArgs>(args)...));
       }

       template <typename T, typename E, typename F, typename... FArgs>
       typename std::enable_if<!std::is_class<T>::value, void>::type
           connect(E &&e, T* t, F && arg0, FArgs&&... args)
       {
           connect(std::forward<E>(e), easy_bind(t, arg0, std::forward<FArgs>(args)...));
       }

       template <typename T, typename E, typename F, typename... Args>
       typename std::enable_if<!std::is_class<T>::value, void>::type
           connect(Event<Args...> &e, T* t)
       {
           auto f = new EventDelegateFunctionPointer<T, Args...>(e, t);
           connect<Args...>(e, f);
       }

       template <typename T, typename... Args>
       void connect(Event<Args...> &e, T* t, void (T::*fn)(Args...))
       {
           auto f = new EventDelegateMemberFunction<T, Args...>(e, t, fn);
           connect<Args...>(e, f);
       }

       template<typename F, typename... Args>
       void connect(Event<Args...> &e, F && fn)
       {
           // As this is either std::bind result
           // Or a lambda we can just pass it to the std::function overload
           //
           std::function<void(Args...)> f = fn;
           connect(e, f);
       }

       template<typename... Args>
       void connect(Event<Args...> &e, std::function<void(Args...)> fn)
       {
           auto f = new EventDelegateFunctionObject<Args...>(e, fn);
           connect<Args...>(e, f);
       }

       template<typename... Args>
       void connect(Event<Args...> &e, EventDelegate<Args...> *ed)
       {
           e.connect(*ed);
           v.push_back(ed);
       }

    private:
       std::vector<EventDelegateBase*> v;
    };
} // ksignals::
