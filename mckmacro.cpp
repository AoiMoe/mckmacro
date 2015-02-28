/*-
 * Copyright (c)2006-2015 Takuya SHIOZAKI,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <set>
#include <list>
#include <utility>
#include <cstdlib>
#include <cctype>
#include <exception>

using namespace std;

#define COPYABLENESS_(name, designator)					\
public:									\
	name(const name &) = designator;				\
	name &operator = (const name &) = designator
#define MOVABLENESS_(name, designator)					\
public:									\
	name(name &&) = designator;					\
	name &operator = (name &&) = designator

#define NONCOPYABLE(name)		COPYABLENESS_(name, delete)
#define DEFAULT_COPYABLE(name)		COPYABLENESS_(name, default)
#define NONMOVABLE(name)		MOVABLENESS_(name, delete)
#define DEFAULT_MOVABLE(name)		MOVABLENESS_(name, default)

template <typename T_>
typename std::enable_if<std::is_pointer<T_>::value, T_>::type
reset_pointer(T_ *pp, T_ &&v = nullptr)
{
	T_ tmp = std::move(*pp);
	*pp = std::forward<T_>(v);
	return tmp;
}

class Exit
{
	int m_code;
public:
	Exit(int code) noexcept : m_code(code) { }
	~Exit() = default;
	int get_code() const noexcept { return m_code; }
};

class ExitSuccess : public Exit
{
public:
	ExitSuccess() noexcept : Exit(EXIT_SUCCESS) { }
	~ExitSuccess() = default;
};

class ExitFailure : public Exit
{
public:
	ExitFailure() noexcept : Exit(EXIT_FAILURE) { }
	~ExitFailure() = default;
};

template <typename Tag_>
class NameError
{
	string m_name;
public:
	NameError(string name) : m_name(std::move(name)) { }
	~NameError() = default;
	const string &get_name() const noexcept
	{
		return m_name;
	}
};

class SimpleEx
{
	string m_message;
public:
	SimpleEx(string str) : m_message(std::move(str)) { }
	~SimpleEx() = default;
	const string &what() const noexcept { return m_message; }
};

constexpr auto DIRECTIVE_CHAR = '#';
constexpr auto MACRO_CHAR = '\\';
constexpr auto MACRO_DEF_CHAR = '\\';
constexpr auto MACRO_UNDEF_CHAR = '!';
constexpr auto INCLUDE_CHAR = '<';
constexpr auto COMM_CHAR = ';';
constexpr auto QUOTE_CHAR = '\"';
constexpr auto PATH_SEP = '\\';
constexpr auto SCOPE_CHAR = ':';
constexpr auto SCOPE_AUTO_ON = '+';
constexpr auto SCOPE_AUTO_OFF = '-';

template <typename Record_>
class LoopDetector
{
	NONCOPYABLE(LoopDetector);
	DEFAULT_MOVABLE(LoopDetector);
private:
	struct LoopedTag { };
public:
	using Looped = NameError<LoopedTag>;
	using Stack = std::list<Record_>;
private:
	using Set = std::set<string>;
	Set m_set;
	Stack m_stack;
	bool m_freeze;
public:
	LoopDetector() noexcept : m_freeze(false) { }
	~LoopDetector() = default;
	bool is_loop(const string &name) const noexcept
	{
		return m_set.find(name) != m_set.end();
	}
	template <typename... Args>
	void push(string name, Args&&... args)
	{
		if (this->is_loop(name)) {
			this->freeze();
			throw Looped(std::move(name));
		}
		m_set.emplace(std::move(name));
		m_stack.emplace_front(std::forward<Args>(args)...);
	}
	void pop() noexcept
	{
		if (m_freeze || m_stack.empty())
			return;
		m_set.erase(*m_stack.begin());
		m_stack.pop_front();
	}
	const Record_ &get_top() const noexcept
	{
		return *m_stack.begin();
	}
	void freeze() noexcept
	{
		m_freeze = true;
	}
	void unfreeze() noexcept
	{
		m_freeze = false;
	}
	const Stack &get_stack() const noexcept
	{
		return m_stack;
	}
	void clear()
	{
		Set().swap(m_set);
		Stack().swap(m_stack);
		m_freeze = false;
	}
};

class MacroStorage
{
	NONCOPYABLE(MacroStorage);
	DEFAULT_MOVABLE(MacroStorage);
private:
	struct UndefinedTag { };
public:
	using Undefined = NameError<UndefinedTag>;
	class Record
	{
		DEFAULT_COPYABLE(Record);
		DEFAULT_MOVABLE(Record);
	private:
		string m_file;
		int m_line;
		string m_contents;
	public:
		Record() noexcept : m_line(0) { }
		Record(string file, int line, string contents)
			: m_file(std::move(file)),
			  m_line(line),
			  m_contents(std::move(contents))
		{
		}
		~Record() = default;
		const string &get_file() const noexcept
		{
			return m_file;
		}
		int get_line() const noexcept
		{
			return m_line;
		}
		const string &get_contents() const noexcept
		{
			return m_contents;
		}
	};
private:
	using Mapper = map<string, Record>;
	Mapper m_mapper;
public:
	MacroStorage() = default;
	~MacroStorage() = default;
	void undef(const string &name)
	{
		auto i = m_mapper.find(name);
		if (i != m_mapper.end())
			m_mapper.erase(i);
	}
	void define(string name, string file, int line, string contents)
	{
		m_mapper.emplace(std::move(name),
				 Record(std::move(file),
					std::move(line),
					std::move(contents)));
	}
	void clear()
	{
		Mapper().swap(m_mapper);
	}
	const Record &query(const string &name) const
	{
		auto i = m_mapper.find(name);
		if (i == m_mapper.end())
			throw Undefined(name);
		return i->second;
	}
};

template <class Container_, class Iter_=typename Container_::const_iterator>
class Region
{
	DEFAULT_COPYABLE(Region);
	DEFAULT_MOVABLE(Region);
public:
	using ValueType = typename Iter_::value_type;
private:
	Iter_ m_curpos, m_end;
	void ensure_not_end_(string funcname) const
	{
		if (m_curpos == m_end)
			throw SimpleEx("Region::"+
				       std::move(funcname)+
				       ": internal error.");
	}
public:
	Region() = default;
	Region(Container_ &container)
		: m_curpos(container.begin()), m_end(container.end())
	{
	}
	Region(Iter_ begin, Iter_ end)
		: m_curpos(begin), m_curpos(end)
	{
	}
	Region(const Region &r1, const Region &r2)
		: m_curpos(r1.curpos()), m_end(r2.curpos())
	{
	}
	Region &operator = (Container_ &container)
	{
		m_curpos = container.begin();
		m_end = container.end();
		return *this;
	}
	bool operator == (const Region &r) const
	{
		return m_curpos == r.curpos() && m_end == r.end();
	}
	~Region() = default;
	bool is_end() const noexcept
	{
		return m_curpos == m_end;
	}
	const Region &operator ++ ()
	{
		ensure_not_end_("++");
		++m_curpos;
		return *this;
	}
	const Region &operator -- ()
	{
		ensure_not_end_("--");
		--m_curpos;
		return *this;
	}
	Region operator ++ (int)
	{
		auto tmp = *this;
		++(*this);
		return tmp;
	}
	Region operator -- (int)
	{
		auto tmp = *this;
		--(*this);
		return tmp;
	}
	ValueType operator * () const
	{
		ensure_not_end_("operator * ()");
		return *m_curpos;
	}
	operator Container_ () const
	{
		return Container_(m_curpos, m_end);
	}
	Iter_ curpos() const
	{
		return m_curpos;
	}
	Iter_ end() const
	{
		return m_end;
	}
	int length() const
	{
		return m_end-m_curpos;
	}
};
using ConstStringRegion = Region<const string>;

class MacroProcessor
{
	NONCOPYABLE(MacroProcessor);
	DEFAULT_MOVABLE(MacroProcessor);
private:
	using Record = MacroStorage::Record;
	using LoopDet = LoopDetector<string>;
public:
	using Stack = LoopDet::Stack;
	using Looped = LoopDet::Looped;
	using Undefined = MacroStorage::Undefined;
private:
	MacroStorage m_storage;
	LoopDet m_loop_detector;
	bool m_auto_scope;
	string m_current_scope;
	string make_scoped_(const string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (string() +
				SCOPE_CHAR +
				m_current_scope +
				SCOPE_CHAR +
				name);
		return name;
	}
	string make_global_(const string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (string() +
				SCOPE_CHAR +
				SCOPE_CHAR +
				name);
		return name;
	}
public:
	MacroProcessor() noexcept : m_auto_scope(false) { }
	~MacroProcessor() = default;
private:
	class Locker
	{
		NONCOPYABLE(Locker);
		friend class MacroProcessor;
		Locker &operator = (Locker &&) = delete;
	private:
		LoopDet &m_loop_detector;
		const Record *m_result;
		Locker(LoopDet &l, const Record &r) noexcept
			: m_loop_detector(l), m_result(&r)
		{
		}
	public:
		Locker(Locker &&rh)
			: m_loop_detector(rh.m_loop_detector),
			  m_result(reset_pointer(&rh.m_result))
		{
		}
		~Locker()
		{
			if (m_result)
				m_loop_detector.pop();
		}
		const string &query() const
		{
			if (!m_result)
				throw SimpleEx("internal error.");
			return m_result->get_contents();
		}
	};
	const Record &query_and_lock_1_(const string &name)
	{
		const auto &r = m_storage.query(name);
		m_loop_detector.push(name, name);
		return r;
	}
	const Record &query_and_lock_(const string &name)
	{
		try {
			return query_and_lock_1_(make_scoped_(name));
		}
		catch (Undefined &) {
			try {
				return query_and_lock_1_(make_global_(name));
			}
			catch (Undefined &) {
				throw Undefined(name);
			}
		}
		catch (...) {
			throw;
		}
	}
public:
	Locker lock(const string &name)
	{
		return Locker(m_loop_detector, query_and_lock_(name));
	}
	void set_scope(string s)
	{
		m_current_scope = std::move(s);
	}
	void set_auto_scope_mode(bool mode)
	{
		m_auto_scope = mode;
	}
	bool is_auto_scope() const
	{
		return m_auto_scope;
	}
	void undef(string name)
	{
		m_storage.undef(make_scoped_(std::move(name)));
	}
	void define(string name, string file, int line, string contents)
	{
		m_storage.define(make_scoped_(std::move(name)),
				 std::move(file),
				 line,
				 std::move(contents));
	}
	void clear()
	{
		m_storage.clear();
		m_loop_detector.clear();
	}
	const Stack &get_stack() const
	{
		return m_loop_detector.get_stack();
	}
	const Record &query(string name) const
	{
		return m_storage.query(make_scoped_(std::move(name)));
	}
};

class PathList
{
	NONCOPYABLE(PathList);
	DEFAULT_MOVABLE(PathList);
private:
	using List = list<string>;
	List m_list;
	struct CannotOpenTag { };
public:
	using CannotOpen = NameError<CannotOpenTag>;
	PathList()
	{
		m_list.emplace_front(".");
	}
	~PathList() = default;
	void push(const string &path)
	{
		if (path.empty())
			return;
		auto tmp = path.end();
		if (*--tmp != PATH_SEP)
			tmp = path.end();
		m_list.emplace_front(path.begin(), tmp);
	}
	void open(ifstream &ifs, const string &name)
	{
		if (name.empty())
			throw CannotOpen("");
		if (*name.begin() == PATH_SEP) {
			ifs.close();
			ifs.clear();
			ifs.open(name.c_str());
			if (!ifs.is_open())
				throw CannotOpen(name);
			return;
		}
		for (auto const &i : m_list) {
			ifs.close();
			ifs.clear();
			ifs.open((i+PATH_SEP+name).c_str());
			if (ifs.is_open())
				return;
		}
		throw CannotOpen(name);
	}
};

class IncludeProcessor
{
	NONCOPYABLE(IncludeProcessor);
	DEFAULT_MOVABLE(IncludeProcessor);
public:
	class Record
	{
		string m_file;
		string m_base_file;
		int m_base_line;
	public:
		Record(const Record &) = default;
		Record(Record &&) = default;
		Record &operator = (const Record &) = default;
		Record &operator = (Record &&) = default;
		Record() : m_base_line(0) { }
		Record(string file, string base_file, int base_line)
			: m_file(std::move(file)),
			  m_base_file(std::move(base_file)),
			  m_base_line(base_line)
		{
		}
		~Record() = default;
		operator const string & () const noexcept
		{
			return m_file;
		}
		const string &get_name() const noexcept
		{
			return m_file;
		}
		const string &get_file() const noexcept
		{
			return m_file;
		}
		const string &get_base_file() const noexcept
		{
			return m_base_file;
		}
		int get_base_line() const noexcept
		{
			return m_base_line;
		}
	};
private:
	LoopDetector<Record> m_loop_detector;
	PathList m_path_list;
public:
	using Stack = LoopDetector<Record>::Stack;
	using Looped = LoopDetector<Record>::Looped;
	using CannotOpen = PathList::CannotOpen;
	IncludeProcessor() = default;
	~IncludeProcessor() = default;
private:
	class Locker
	{
		NONCOPYABLE(Locker);
		friend class IncludeProcessor;
		Locker &operator = (Locker &&) = delete;
	private:
		IncludeProcessor *m_ip;
		Locker(IncludeProcessor &ip,
		       string name,
		       string base_file="",
		       int base_line=0) : m_ip(&ip)
		{
			try {
				m_ip->m_loop_detector.push(
					name,
					name,
					std::move(base_file),
					base_line);
			}
			catch (Looped &ex) {
				throw;
			}
		}
	public:
		Locker(Locker &&rh) : m_ip(reset_pointer(&rh.m_ip)) { }
		~Locker()
		{
			if (m_ip)
				m_ip->m_loop_detector.pop();
		}
	};
public:
	Locker lock(string name, string base_file="", int base_line=0)
	{
		return Locker(*this,
			      std::move(name),
			      std::move(base_file),
			      base_line);
	}
	void push(string path)
	{
		m_path_list.push(std::move(path));
	}
	void open(ifstream &ifs, string name)
	{
		m_path_list.open(ifs, std::move(name));
	}
	const Stack &get_stack() const
	{
		return m_loop_detector.get_stack();
	}
};

class FileContext
{
	NONCOPYABLE(FileContext);
	NONMOVABLE(FileContext);
private:
	MacroProcessor &m_macro_processor;
	IncludeProcessor &m_include_processor;
	ostream &m_logger;
	string m_input_file_name;
	istream &m_input_stream;
	string m_output_file_name;
	ostream &m_output_stream;
	int m_line_number;
//
	FileContext(MacroProcessor &macro_processor,
		    IncludeProcessor &include_processor,
		    ostream &logger,
		    string input_file_name,
		    istream &input_stream,
		    string output_file_name,
		    ostream &output_stream)
		: m_macro_processor(macro_processor),
		  m_include_processor(include_processor),
		  m_logger(logger),
		  m_input_file_name(std::move(input_file_name)),
		  m_input_stream(input_stream),
		  m_output_file_name(std::move(output_file_name)),
		  m_output_stream(output_stream),
		  m_line_number(0)
	{
	}
	void do_define_macro_(ConstStringRegion) const;
	void do_undef_macro_(ConstStringRegion) const;
	void do_include_(ConstStringRegion) const;
	void do_set_scope_(ConstStringRegion) const;
	static bool skip_macro_directive_chars_(ConstStringRegion *input);
	bool directive_(ConstStringRegion,
			void (FileContext::*)(ConstStringRegion) const,
			const char) const;
	bool define_macro_(const ConstStringRegion &input) const
	{
		return directive_(input, &FileContext::do_define_macro_,
				  MACRO_DEF_CHAR);
	}
	bool undef_macro_(const ConstStringRegion &input) const
	{
		return directive_(input, &FileContext::do_undef_macro_,
				  MACRO_UNDEF_CHAR);
	}
	bool set_scope_(const ConstStringRegion &input) const
	{
		return directive_(input, &FileContext::do_set_scope_,
				  SCOPE_CHAR);
	}
	bool include_(const ConstStringRegion &input) const
	{
		return directive_(input, &FileContext::do_include_,
				  INCLUDE_CHAR);
	}
	string expand_(ConstStringRegion) const;
	void process_();
public:
	~FileContext() = default;
	MacroProcessor &get_macro_processor() const noexcept
	{
		return m_macro_processor;
	}
	void put_message(string fac, string msg) const
	{
		m_logger << std::move(fac)
			 << " at line " << m_line_number << " in "
			 << m_input_file_name << ": "
			 << std::move(msg) << std::endl;
	}
	static void process(MacroProcessor &macro_processor,
			    IncludeProcessor &include_processor,
			    ostream &logger,
			    string input_name, istream &input_stream,
			    string output_name, ostream &output_stream)
	{
		FileContext ctx(macro_processor, include_processor, logger,
				std::move(input_name), input_stream,
				std::move(output_name), output_stream);
		ctx.process_();
	}
};

void
skip_ws(ConstStringRegion *pr)
{
	ConstStringRegion &r=*pr;
	for (; !r.is_end(); ++r)
		if (!isspace(*r))
			break;
}

string
get_macro_name(ConstStringRegion *pr, bool *rscoped = NULL)
{
	auto &r = *pr;
	const auto saved = r;
	auto scoped = false;
	auto scoped_done = false;
	auto body=false;
	string ret;

	for (; !r.is_end(); ++r) {
		if (r == saved && *r == SCOPE_CHAR)
			scoped = true;
		else if (scoped && !scoped_done) {
			if (*r == SCOPE_CHAR)
				scoped_done = true;
			else if (!(isalnum(*r) || *r == '_'))
				break;
		} else if (!(isupper(*r) || *r == '_' || isdigit(*r)))
			break;
		else if (!scoped || scoped_done)
			body = true;
	}
	if (scoped && !scoped_done)
		throw SimpleEx("ill-formed macro scope.");
	if (!body)
		throw SimpleEx("cannot get macro name.");
	if (rscoped)
		*rscoped = scoped;
	return ConstStringRegion(saved, r);
}

string
get_scope_name(ConstStringRegion *pr)
{
	auto &r = *pr;
	const auto saved = r;

	for (; !r.is_end(); ++r) {
		if (!(isalnum(*r) || *r == '_'))
			break;
	}

	return ConstStringRegion(saved, r);
}

string
get_string(ConstStringRegion input, bool *quoted = NULL)
{
	ConstStringRegion begin, end;
	char quote_char = 0;

	if (quoted)
		*quoted = false;
	skip_ws(&input);

	if (input.is_end())
		return input;

	begin = end = input;
	for (; !input.is_end(); ++input) {
		if (*input == QUOTE_CHAR) {
			if (begin == input) {
				quote_char = *input;
				++begin;
				continue;
			} else {
				if (!quote_char)
					throw SimpleEx("unexpected quote "
						       "character.");
				quote_char = 0;
				++input;
				skip_ws(&input);
				if (quoted)
					*quoted = true;
				if (!input.is_end() &&
				    *input != COMM_CHAR)
					throw SimpleEx("unexpected character "
						       "after quotation.");
				break;
			}
		}
		if (*input == COMM_CHAR)
			break;
		if (quote_char || !isspace(*input))
			end = input;
	}
	if (quote_char)
		throw SimpleEx("unclosed quotation.");

	return string(ConstStringRegion(begin, ++end));
}

void
FileContext::do_define_macro_(ConstStringRegion input) const
{
	string name;
	string additional = " "; // default: add one space character.
	string str;
	auto quoted = false;

	name = get_macro_name(&input);
	str = get_string(input, &quoted);
	if (str.empty() && !quoted) {
//		m_macro_processor.undef(name);
		m_macro_processor.define(name,
					 m_input_file_name, m_line_number, "");
	} else {
		if (quoted)
			additional = "";
		m_macro_processor.define(name,
					 m_input_file_name, m_line_number,
					 str + additional);
	}
}

void
FileContext::do_undef_macro_(ConstStringRegion input) const
{
	m_macro_processor.undef(get_macro_name(&input));
}

void
FileContext::do_include_(ConstStringRegion input) const
{
	ifstream ifs;

	auto file = get_string(input);
	m_include_processor.open(ifs, file);

	auto locker = m_include_processor.lock(
		file, m_input_file_name, m_line_number);
	FileContext::process(m_macro_processor, m_include_processor, m_logger,
			     file, ifs, m_output_file_name, m_output_stream);
}

void
FileContext::do_set_scope_(ConstStringRegion input) const
{
	if (!input.is_end()) {
		switch (*input) {
		case SCOPE_AUTO_ON:
			m_macro_processor.set_auto_scope_mode(true);
			return;
		case SCOPE_AUTO_OFF:
			m_macro_processor.set_auto_scope_mode(false);
			return;
		}
	}
	m_macro_processor.set_scope(get_scope_name(&input));
}

bool
FileContext::skip_macro_directive_chars_(ConstStringRegion *input)
{
	skip_ws(input);
	return input->length() > 1 && *(*input)++ == DIRECTIVE_CHAR;
}

bool
FileContext::directive_(ConstStringRegion input,
			void (FileContext::*dofunc_)(ConstStringRegion) const,
			const char dirchar) const
{

	if (skip_macro_directive_chars_(&input) && *input == dirchar) {
		++input;
		(this->*dofunc_)(input);
		return true;
	}
	return false;
}

string
FileContext::expand_(ConstStringRegion input) const
{
	auto dbcs = false;
	auto macro_char = false;
	auto enter_out = false;
	string out;

	auto begin = input;
	auto saved = input;
	while (!input.is_end()) {
		if (dbcs) {
			dbcs = false;
			goto next;
		}
		if ((*input & 0x80) != 0) {
			dbcs = true;
			goto next;
		}
		if (macro_char) {
			macro_char = false;
			if (*input == MACRO_CHAR || *input == COMM_CHAR) {
				auto tmp = input;
				out += string(ConstStringRegion(begin, --tmp));
				out += *input;
				enter_out = true;
				begin = ++input;
				continue;
			}
			auto tmp = input;
			out += string(ConstStringRegion(begin, --tmp));
			enter_out = true;
			try {
				auto locker = m_macro_processor.lock(
					get_macro_name(&input));
				out += this->expand_(locker.query());
			}
			catch (MacroProcessor::Undefined &ex) {
#if 1
				throw SimpleEx(string("macro \"")+
					       MACRO_CHAR+ex.get_name()+
					       "\" is not defined.");
#else
				this->put_message(
					"warning",
					string("macro \"")+
					MACRO_CHAR+ex.get_name()+
					"\" is not defined.");
#endif
			}
			if (!input.is_end() && isspace(*input))
				++input;
			begin = input;
			continue;
		}
		if (*input == MACRO_CHAR)
			macro_char = true;
		if (*input == COMM_CHAR)
			break;
next:
		++input;
	}
	if (enter_out) {
		out += string(begin);
		return out;
	}
	return saved;
}

void
FileContext::process_()
{
	try {
		while (m_input_stream.good()) {
			string input;

			getline(m_input_stream, input);
			m_line_number++;
			if (this->define_macro_(input))
				m_output_stream << endl;
			else if (this->undef_macro_(input))
				m_output_stream << endl;
			else if (this->set_scope_(input))
				m_output_stream << endl;
			else if (this->include_(input))
				;
			else {
				if (m_macro_processor.is_auto_scope() &&
				    input.size() > 0 &&
				    isalpha((int)(unsigned char)input[0])) {
					m_macro_processor.set_scope(
						string(1, input[0]));
				}
				m_output_stream << this->expand_(input)
						<< endl;
			}
			if (m_output_stream.bad())
				throw SimpleEx("cannot write to file \"" +
					       m_output_file_name + "\"");
		}
	}
	catch (SimpleEx &ex) {
		this->put_message("error", ex.what());
		throw ExitFailure();
	}
	catch (MacroProcessor::Looped &ex) {
		this->put_message("error",
				  string("macro ")+MACRO_CHAR+ex.get_name()+
				  " is looped:");
		for (auto &mname : m_macro_processor.get_stack()) {
			auto &rec = m_macro_processor.query(mname);
			m_logger << "\t" << MACRO_CHAR << mname
				 << " defined at line " << rec.get_line()
				 << " in " << rec.get_file()
				 << endl;
		}
		throw ExitFailure();
	}
	catch (IncludeProcessor::Looped &ex) {
		this->put_message("error",
				  "including file \""+ex.get_name()+
				  "\" is looped:");
		for (auto &rec : m_include_processor.get_stack()) {
			if (rec.get_base_file().empty())
				break;
			m_logger << "\t\"" << rec.get_file()
				 << "\" include at line " << rec.get_base_line()
				 << " in \"" << rec.get_base_file() << "\""
				 << endl;
		}
		throw ExitFailure();
	}
	catch (PathList::CannotOpen &ex) {
		this->put_message("error",
				  "file \""+ex.get_name()+
				  "\" cannot be opened.");
		throw ExitFailure();
	}
}

auto f_banner = true;

void
banner()
{
	if (f_banner)
		cerr << "macro for mck (c)2006-2015 T.SHIOZAKI." << endl;
	f_banner = false;
}

void
usage()
{
	banner();
	cerr << "usage: mckmacro [-q] [-o outfile] [infile]" << endl;
	exit(EXIT_FAILURE);
}

void
warnx(const char *fmt)
{
	banner();
	cerr << fmt << endl;
}

void
errx(int excode, const char *fmt)
{
	warnx(fmt);
	exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
	MacroProcessor macro_processor;
	IncludeProcessor include_processor;
	ifstream ifs;
	ofstream ofs;
	istream *is = &cin;
	ostream *os = &cout;
	string ifile;
	string ofile;
	auto done = false;

	argv++;
	argc--;
	while (!done && argc > 0 && *argv[0] == '-') {
		switch (argv[0][1]) {
		case '-':
			done = true;
			break;
		case 'q':
			f_banner = false;
			break;
		case 'o':
			if (argc < 2)
				usage();
			if (ofile.size() > 0)
				errx(EXIT_FAILURE,
				     "error: duplicate -o option.");
			argv++;
			argc--;
			ofile = argv[0];
			break;
		default:
			warnx((string("error: unknown option -")+
			       string(1, argv[0][1])+".").c_str());
			usage();
		}
		argv++;
		argc--;
	}
	if (argc > 1) {
		warnx("error: too many input files.");
		usage();
	} else if (argc == 1)
		ifile = argv[0];

	if (ifile.size() > 0) {
		ifs.open(ifile.c_str());
		if (ifs.fail())
			errx(EXIT_FAILURE,
			     (string("error: cannot open file \"")+
			      ifile+"\".").c_str());
		is = &ifs;
	} else {
		ifile = "<stdin>";
	}
	if (ofile.size() > 0) {
		ofs.open(ofile.c_str());
		if (ofs.fail())
			errx(EXIT_FAILURE,
			     (string("error: cannot open file \"")+
			      ofile+"\".").c_str());
		os = &ofs;
	} else {
		ofile = "<stdout>";
	}
	banner();
	try {
		auto locker = include_processor.lock(ifile);
		FileContext::process(macro_processor, include_processor, cerr,
				     ifile, *is, ofile, *os);
	} catch (Exit &ex) {
		return ex.get_code();
	}

	return EXIT_SUCCESS;
}
