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
#include <type_traits>
#include <functional>

#define COPYABLENESS_(name, designator)					\
	name(const name &) = designator;				\
	name &operator = (const name &) = designator
#define MOVABLENESS_(name, designator)					\
	name(name &&) = designator;					\
	name &operator = (name &&) = designator

#define NONCOPYABLE(name)		COPYABLENESS_(name, delete)
#define DEFAULT_COPYABLE(name)		COPYABLENESS_(name, default)
#define NONMOVABLE(name)		MOVABLENESS_(name, delete)
#define DEFAULT_MOVABLE(name)		MOVABLENESS_(name, default)

//
// user-defined literal : "xxxx"_s -> std::string
//
std::string operator "" _s (const char *str, std::size_t len)
{
	return std::string(str, len);
}

//
// reset_pointer(pp, v=nullptr) : release and set the pointer.
//
// pp     : pointer to the pointer to be reset.
// v      : (optional) the value to be set to *pp, nullptr by default.
// return : old value of *pp;
//
template <typename T_>
typename std::enable_if<std::is_pointer<T_>::value, T_>::type
reset_pointer(T_ *pp, T_ v = nullptr) noexcept
{
	std::swap(*pp, v);
	return v;
}

//
// Exit : exception expressing global exit.
//
class Exit
{
	int m_code;
public:
	~Exit() = default;
	Exit(int code) noexcept : m_code(code) { }
	int get_code() const noexcept { return m_code; }
};

class ExitSuccess : public Exit
{
public:
	~ExitSuccess() = default;
	ExitSuccess() noexcept : Exit(EXIT_SUCCESS) { }
};

class ExitFailure : public Exit
{
public:
	~ExitFailure() = default;
	ExitFailure() noexcept : Exit(EXIT_FAILURE) { }
};

//
// NameError : exception expressing name-related error.
//
template <typename Tag_>
class NameError
{
	std::string m_name;
public:
	~NameError() = default;
	NameError(std::string name) noexcept : m_name(std::move(name)) { }
	const std::string &get_name() const noexcept
	{
		return m_name;
	}
};

//
// SimpleEx : exception containing some message.
//
template <typename Tag_>
class SimpleEx
{
	std::string m_message;
public:
	SimpleEx(std::string str) noexcept : m_message(std::move(str)) { }
	~SimpleEx() = default;
	const std::string &what() const noexcept { return m_message; }
};

struct SyntaxErrorTag { };
using SyntaxError = SimpleEx<SyntaxErrorTag>;

struct FatalErrorTag { };
using FatalError = SimpleEx<FatalErrorTag>;

// tokens
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

//
// LoopDetector : generic loop detector.
//
// elements:
//   Set   : set of record names to check duplication.
//   Stack : record stack to be traced back.
//
template <typename Record_>
class LoopDetector
{
public:
	NONCOPYABLE(LoopDetector);
	DEFAULT_MOVABLE(LoopDetector);
private:
	struct LoopedTag { };
public:
	using Looped = NameError<LoopedTag>;
	using Stack = std::list<Record_>;
public:
	~LoopDetector() = default;
	LoopDetector() = default;
	bool is_loop(const std::string &name) const noexcept
	{
		return m_set.find(name) != m_set.end();
	}
	template <typename... Args>
	void push(std::string name, Args&&... args)
	{
		if (this->is_loop(name)) {
			throw Looped(std::move(name));
		}
		m_set.emplace(std::move(name));
		m_stack.emplace_front(std::forward<Args>(args)...);
	}
	void pop() noexcept
	{
		if (m_stack.empty())
			return;
		try {
			m_set.erase(*m_stack.begin());
			m_stack.pop_front();
		}
		catch (...) {
		}
	}
	const Record_ &get_top() const noexcept
	{
		return *m_stack.begin();
	}
	const Stack &get_stack() const noexcept
	{
		return m_stack;
	}
	void clear() noexcept
	{
		m_set.clear();
		m_stack.clear();
	}
private:
	using Set = std::set<std::string>;
	Set m_set;
	Stack m_stack;
};

//
// MacroStorage : associate memory among macro name and its contents.
//
class MacroStorage
{
public:
	NONCOPYABLE(MacroStorage);
	DEFAULT_MOVABLE(MacroStorage);
private:
	struct UndefinedTag { };
public:
	using Undefined = NameError<UndefinedTag>;
	class Record
	{
	public:
		DEFAULT_COPYABLE(Record);
		DEFAULT_MOVABLE(Record);
		~Record() = default;
		Record() = default;
		Record(std::string file,
		       int line,
		       std::string contents) noexcept
			: m_file(std::move(file)),
			  m_line(line),
			  m_contents(std::move(contents))
		{
		}
		const std::string &get_file() const noexcept
		{
			return m_file;
		}
		int get_line() const noexcept
		{
			return m_line;
		}
		const std::string &get_contents() const noexcept
		{
			return m_contents;
		}
	private:
		std::string m_file;
		int m_line = 0;
		std::string m_contents;
	};
public:
	~MacroStorage() = default;
	MacroStorage() = default;
	void undef(const std::string &name)
	{
		auto i = m_mapper.find(name);
		if (i != m_mapper.end())
			m_mapper.erase(i);
	}
	void define(std::string name, std::string file, int line,
		    std::string contents)
	{
		m_mapper.emplace(std::move(name),
				 Record(std::move(file),
					std::move(line),
					std::move(contents)));
	}
	void clear() noexcept
	{
		m_mapper.clear();
	}
	const Record &query(const std::string &name) const
	{
		auto i = m_mapper.find(name);
		if (i == m_mapper.end())
			throw Undefined(name);
		return i->second;
	}
private:
	using Mapper = std::map<std::string, Record>;
	Mapper m_mapper;
};

//
// Region : region of any sequencial container.
//
template <class Container_, class Iter_=typename Container_::const_iterator>
class Region
{
public:
	DEFAULT_COPYABLE(Region);
	DEFAULT_MOVABLE(Region);
	using ValueType = typename Iter_::value_type;
public:
	~Region() = default;
	Region() = default;
	Region(Container_ &container) noexcept
		: m_curpos(container.begin()), m_end(container.end())
	{
	}
	Region(Iter_ begin, Iter_ end) noexcept
		: m_curpos(begin), m_curpos(end)
	{
	}
	Region(const Region &r1, const Region &r2) noexcept
		: m_curpos(r1.curpos()), m_end(r2.curpos())
	{
	}
	Region &operator = (Container_ &container) noexcept
	{
		m_curpos = container.begin();
		m_end = container.end();
		return *this;
	}
	bool operator == (const Region &r) const noexcept
	{
		return m_curpos == r.curpos() && m_end == r.end();
	}
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
	Iter_ curpos() const noexcept
	{
		return m_curpos;
	}
	Iter_ end() const noexcept
	{
		return m_end;
	}
	int length() const noexcept
	{
		return m_end-m_curpos;
	}
private:
	void ensure_not_end_(std::string funcname) const
	{
		if (m_curpos == m_end)
			throw FatalError("Region::"+
					 std::move(funcname)+
					 ": internal error.");
	}
private:
	Iter_ m_curpos, m_end;
};
using ConstStringRegion = Region<const std::string>;


//
// MacroProcessor : keep macro definitions and expand it by request.
//
class MacroProcessor
{
public:
	NONCOPYABLE(MacroProcessor);
	DEFAULT_MOVABLE(MacroProcessor);
private:
	using Record = MacroStorage::Record;
	using LoopDet = LoopDetector<std::string>;
public:
	using Stack = LoopDet::Stack;
	using Looped = LoopDet::Looped;
	using Undefined = MacroStorage::Undefined;
private:
	class Locker
	{
		friend class MacroProcessor;
		NONCOPYABLE(Locker);
		Locker &operator = (Locker &&) = delete;
	public:
		~Locker() noexcept
		{
			if (m_result)
				m_loop_detector.pop();
		}
		Locker(Locker &&rh) noexcept
			: m_loop_detector(rh.m_loop_detector),
			  m_result(reset_pointer(&rh.m_result))
		{
		}
		const std::string &query() const
		{
			if (!m_result)
				throw FatalError("internal error.");
			return m_result->get_contents();
		}
	private:
		Locker(LoopDet &l, const Record &r) noexcept
			: m_loop_detector(l), m_result(&r)
		{
		}
		LoopDet &m_loop_detector;
		const Record *m_result;
	};
public:
	~MacroProcessor() = default;
	MacroProcessor() = default;
	Locker lock(const std::string &name)
	{
		return Locker(m_loop_detector, query_and_lock_(name));
	}
	void set_scope(std::string s) noexcept
	{
		m_current_scope = std::move(s);
	}
	void undef(std::string name)
	{
		m_storage.undef(make_scoped_(std::move(name)));
	}
	void define(std::string name, std::string file, int line,
		    std::string contents)
	{
		m_storage.define(make_scoped_(std::move(name)),
				 std::move(file),
				 line,
				 std::move(contents));
	}
	void clear() noexcept
	{
		m_storage.clear();
		m_loop_detector.clear();
	}
	const Stack &get_stack() const noexcept
	{
		return m_loop_detector.get_stack();
	}
	const Record &query(std::string name) const
	{
		return m_storage.query(make_scoped_(std::move(name)));
	}
private:
	std::string make_scoped_(const std::string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (std::string() +
				SCOPE_CHAR +
				m_current_scope +
				SCOPE_CHAR +
				name);
		return name;
	}
	std::string make_global_(const std::string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (std::string() +
				SCOPE_CHAR +
				SCOPE_CHAR +
				name);
		return name;
	}
	const Record &query_and_lock_1_(const std::string &name)
	{
		const auto &r = m_storage.query(name);
		m_loop_detector.push(name, name);
		return r;
	}
	const Record &query_and_lock_(const std::string &name)
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
private:
	MacroStorage m_storage;
	LoopDet m_loop_detector;
	std::string m_current_scope;
};


//
// PathList : search path list
//
class PathList
{
public:
	NONCOPYABLE(PathList);
	DEFAULT_MOVABLE(PathList);
private:
	using List = std::list<std::string>;
	struct CannotOpenTag { };
public:
	using CannotOpen = NameError<CannotOpenTag>;
	PathList()
	{
		m_list.emplace_front(".");
	}
	~PathList() = default;
	void push(const std::string &path)
	{
		if (path.empty())
			return;
		auto tmp = path.end();
		if (*--tmp != PATH_SEP)
			tmp = path.end();
		m_list.emplace_front(path.begin(), tmp);
	}
	void open(std::ifstream &ifs, const std::string &name)
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
private:
	List m_list;
};

//
// IncludeProcessor : processing including source files.
//
//   - keep path list in which include files are searched.
//   - open include file with loop detection.
//
class IncludeProcessor
{
public:
	NONCOPYABLE(IncludeProcessor);
	DEFAULT_MOVABLE(IncludeProcessor);
private:
	class Locker;
public:
	class Record
	{
	public:
		DEFAULT_COPYABLE(Record);
		DEFAULT_MOVABLE(Record);
		~Record() = default;
		Record() = default;
		Record(std::string file,
		       std::string base_file,
		       int base_line) noexcept
			: m_file(std::move(file)),
			  m_base_file(std::move(base_file)),
			  m_base_line(base_line)
		{
		}
		operator const std::string & () const noexcept
		{
			return m_file;
		}
		const std::string &get_name() const noexcept
		{
			return m_file;
		}
		const std::string &get_file() const noexcept
		{
			return m_file;
		}
		const std::string &get_base_file() const noexcept
		{
			return m_base_file;
		}
		int get_base_line() const noexcept
		{
			return m_base_line;
		}
	private:
		std::string m_file;
		std::string m_base_file;
		int m_base_line = 0;
	};
public:
	using Stack = LoopDetector<Record>::Stack;
	using Looped = LoopDetector<Record>::Looped;
	using CannotOpen = PathList::CannotOpen;
private:
	class Locker
	{
		friend class IncludeProcessor;
		NONCOPYABLE(Locker);
		Locker &operator = (Locker &&) = delete;
	public:
		~Locker() noexcept
		{
			if (m_ip)
				m_ip->m_loop_detector.pop();
		}
		Locker(Locker &&rh) noexcept : m_ip(reset_pointer(&rh.m_ip)) { }
	private:
		Locker(IncludeProcessor &ip, std::string name,
		       std::string base_file, int base_line) : m_ip(&ip)
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
		IncludeProcessor *m_ip;
	};
public:
	~IncludeProcessor() = default;
	IncludeProcessor() = default;
	Locker lock(std::string name, std::string base_file="", int base_line=0)
	{
		return Locker(*this,
			      std::move(name),
			      std::move(base_file),
			      base_line);
	}
	void push(std::string path)
	{
		m_path_list.push(std::move(path));
	}
	void open(std::ifstream &ifs, std::string name)
	{
		m_path_list.open(ifs, std::move(name));
	}
	const Stack &get_stack() const noexcept
	{
		return m_loop_detector.get_stack();
	}
private:
	LoopDetector<Record> m_loop_detector;
	PathList m_path_list;
};


//
// CompileUnitContext : context per compile unit.
//
// compile unit corresponding to an output file, generated from
// a base source file and some include files if necessary.
//
class CompileUnitContext
{
	NONCOPYABLE(CompileUnitContext);
	NONMOVABLE(CompileUnitContext);
public:
	~CompileUnitContext() = default;
	CompileUnitContext(std::string ofname,
			   std::ostream &ofs,
			   std::ostream &lgr) noexcept
		: m_output_file_name(std::move(ofname)),
		  m_output_stream(ofs),
		  m_logger(lgr)
	{
	}
	MacroProcessor &macro_processor() noexcept
	{
		return m_macro_processor;
	}
	IncludeProcessor &include_processor() noexcept
	{
		return m_include_processor;
	}
	std::ostream &logger() noexcept
	{
		return m_logger;
	}
	const std::string &get_output_file_name() const noexcept
	{
		return m_output_file_name;
	}
	std::ostream &output_stream() noexcept
	{
		return m_output_stream;
	}
	void set_auto_scope_mode(bool mode) noexcept
	{
		m_auto_scope = mode;
	}
	bool is_auto_scope() const noexcept
	{
		return m_auto_scope;
	}
	void set_error_as_fatal_mode(bool mode) noexcept
	{
		m_error_as_fatal = mode;
	}
	bool is_error_as_fatal() noexcept
	{
		return m_error_as_fatal;
	}
	void set_warning_as_error_mode(bool mode) noexcept
	{
		m_warning_as_error = mode;
	}
	bool is_warning_as_error() noexcept
	{
		return m_warning_as_error;
	}
	int get_error_count() noexcept
	{
		return m_error_count;
	}
	int get_warn_count() noexcept
	{
		return m_warn_count;
	}
	void incr_error_count() noexcept
	{
		m_error_count++;
	}
	void incr_warn_count() noexcept
	{
		m_warn_count++;
	}
private:
	MacroProcessor m_macro_processor;
	IncludeProcessor m_include_processor;
	std::string m_output_file_name;
	std::ostream &m_output_stream;
	std::ostream &m_logger;
	bool m_error_as_fatal = false;
	bool m_warning_as_error = false;
	bool m_auto_scope = false;
	int m_error_count = 0;
	int m_warn_count = 0;
};

//
// FileContext : context per source file.
//
// file context is corresponding to each source file or include file.
// the instance of this class is generated on the stack recursively
// when the source/include file is opened, and destroyed at closing.
//
class FileContext
{
public:
	NONCOPYABLE(FileContext);
	NONMOVABLE(FileContext);
	using MessageHook = void (std::ostream &);
	using MessageHookVar = std::function<MessageHook>;
	~FileContext() = default;
	MacroProcessor &macro_processor() const noexcept
	{
		return m_compile_unit_context.macro_processor();
	}
	IncludeProcessor &include_processor() const noexcept
	{
		return m_compile_unit_context.include_processor();
	}
	std::ostream &output_stream() const noexcept
	{
		return m_compile_unit_context.output_stream();
	}
	const std::string &get_output_file_name() const noexcept
	{
		return m_compile_unit_context.get_output_file_name();
	}
	std::ostream &logger() const noexcept
	{
		return m_compile_unit_context.logger();
	}
	void put_message(const std::string &fac, const std::string &msg,
			 MessageHookVar additional = noneMessageHook_) const
	{
		logger() << fac << " at line " << m_line_number << " in "
			 << m_input_file_name << ": " << msg << std::endl;
		additional(logger());
	}
	void error(const std::string &msg,
		   MessageHookVar additional = noneMessageHook_) const
	{
		m_compile_unit_context.incr_error_count();
		put_message(s_error, msg, additional);
		if (m_compile_unit_context.is_error_as_fatal())
			throw ExitFailure();
	}
	void warning(const std::string &msg,
		     MessageHookVar additional = noneMessageHook_) const
	{
		if (m_compile_unit_context.is_warning_as_error()) {
			error(msg, additional);
			return;
		}
		m_compile_unit_context.incr_warn_count();
		put_message(s_warning, msg, additional);
	}
	static void process(CompileUnitContext &ctx,
			    std::string input_name,
			    std::istream &input_stream)
	{
		auto locker = ctx.include_processor().lock(input_name);

		FileContext(ctx,
			    std::move(input_name),
			    input_stream).process_();
	}
private:
	using DirectiveHandler = bool (FileContext::*)(ConstStringRegion) const;
	using DirectiveMap = std::map<char, DirectiveHandler>;
	FileContext(CompileUnitContext &cuctx,
		    std::string input_file_name,
		    std::istream &input_stream)
		: m_compile_unit_context(cuctx),
		  m_input_file_name(std::move(input_file_name)),
		  m_input_stream(input_stream)
	{
	}
	bool do_define_macro_(ConstStringRegion) const;
	bool do_undef_macro_(ConstStringRegion) const;
	bool do_include_(ConstStringRegion) const;
	bool do_set_scope_(ConstStringRegion) const;
	static bool skip_macro_directive_chars_(ConstStringRegion *) noexcept;
	bool directive_(ConstStringRegion) const;
	static DirectiveHandler search_directive_handler_(char ch)
	{
		auto i = s_directive_pair.find(ch);

		return i != s_directive_pair.end() ? i->second : nullptr;
	}
	void dump_macro_stack(std::ostream &os) const noexcept
	{
		try {
			MacroProcessor &m = macro_processor();

			for (auto &mname : m.get_stack()) {
				auto &rec = m.query(mname);
				os << "\t" << MACRO_CHAR << mname
				   << " defined at line " << rec.get_line()
				   << " in " << rec.get_file() << std::endl;
			}
		}
		catch (...) {
		}
	}
	void show_include_record_(std::ostream &os,
				  const std::string &file,
				  const std::string &base_file,
				  int base_line) const
	{
		os << '\"' << file << "\" include at line " << base_line
		   << " in \"" << base_file << "\"";
	}
	void dump_include_stack(std::ostream &os) const noexcept
	{
		try {
			IncludeProcessor &i = include_processor();

			for (auto &rec : i.get_stack()) {
				if (rec.get_base_line()) {
					os << '\t';
					show_include_record_(
						os,
						rec.get_file(),
						rec.get_base_file(),
						rec.get_base_line());
					os << std::endl;
				}
			}
		}
		catch (...) {
		}
	}
	std::string expand_(ConstStringRegion) const;
	void process_();
	static void noneMessageHook_(std::ostream &) { }
private:
	CompileUnitContext &m_compile_unit_context;
	std::string m_input_file_name;
	std::istream &m_input_stream;
	int m_line_number = 0;
	static const DirectiveMap s_directive_pair;
	static const std::string s_error;
	static const std::string s_warning;
	static const std::string s_fatal;
};

const FileContext::DirectiveMap FileContext::s_directive_pair = {
	{ MACRO_DEF_CHAR, &FileContext::do_define_macro_ },
	{ MACRO_UNDEF_CHAR, &FileContext::do_undef_macro_ },
	{ SCOPE_CHAR, &FileContext::do_set_scope_ },
	{ INCLUDE_CHAR, &FileContext::do_include_ }
};

const std::string FileContext::s_error = "error";
const std::string FileContext::s_warning = "warning";
const std::string FileContext::s_fatal = "fatal";

namespace
{

//
// utility functions mainly used by FileContext class.
//

//
// skip_ws : skip white space.
//
void
skip_ws(ConstStringRegion *pr) noexcept
{
	auto &r = *pr;
	for (; !r.is_end(); ++r)
		if (!isspace(*r))
			break;
}

//
// get_macro_name : get macro name.
//
std::string
get_macro_name(ConstStringRegion *pr)
{
	auto &r = *pr;
	const auto saved = r;
	auto scoped = false;
	auto scoped_done = false;
	auto body=false;

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
		throw SyntaxError("ill-formed macro scope.");
	if (!body)
		throw SyntaxError("cannot get macro name.");

	return ConstStringRegion(saved, r);
}

//
// get_scope_name : get space name.
//
std::string
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

//
// get_string : get string.
//
std::pair<std::string, bool>
get_string(ConstStringRegion input)
{
	using ReturnType = std::pair<std::string, bool>;

	skip_ws(&input);

	if (input.is_end())
		return ReturnType{input, false};

	auto begin = input, end = input;
	char quote_char = 0;
	bool quoted = false;

	for (; !input.is_end(); ++input) {
		if (*input == QUOTE_CHAR) {
			if (begin == input) {
				quote_char = *input;
				++begin;
				continue;
			} else {
				if (!quote_char)
					throw SyntaxError("unexpected quote "
							  "character.");
				quote_char = 0;
				++input;
				skip_ws(&input);
				quoted = true;
				if (!input.is_end() &&
				    *input != COMM_CHAR)
					throw SyntaxError(
						"unexpected character "
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
		throw SyntaxError("unclosed quotation.");

	return ReturnType{ConstStringRegion(begin, ++end), quoted};
}

} // namespace <anonymous>

bool
FileContext::do_define_macro_(ConstStringRegion input) const
{
	auto additional = " "_s; // default: add one space character

	const auto name = get_macro_name(&input);
	const auto str = get_string(input);
	if (str.first.empty() && !str.second) {
		macro_processor().define(name,
					 m_input_file_name, m_line_number, "");
	} else {
		if (str.second)
			additional = "";
		macro_processor().define(name,
					 m_input_file_name, m_line_number,
					 str.first + additional);
	}
	output_stream() << std::endl;
	return true;
}

bool
FileContext::do_undef_macro_(ConstStringRegion input) const
{
	macro_processor().undef(get_macro_name(&input));
	output_stream() << std::endl;
	return true;
}

bool
FileContext::do_include_(ConstStringRegion input) const
{
	std::ifstream ifs;

	const auto file = get_string(input).first;
	include_processor().open(ifs, file);

	auto locker = include_processor().lock(file,
					       m_input_file_name,
					       m_line_number);

	FileContext(m_compile_unit_context, file, ifs).process_();

	return true;
}

bool
FileContext::do_set_scope_(ConstStringRegion input) const
{
	if (!input.is_end()) {
		switch (*input) {
		case SCOPE_AUTO_ON:
			m_compile_unit_context.set_auto_scope_mode(true);
			return true;
		case SCOPE_AUTO_OFF:
			m_compile_unit_context.set_auto_scope_mode(false);
			return true;
		}
	}
	macro_processor().set_scope(get_scope_name(&input));
	output_stream() << std::endl;
	return true;
}

bool
FileContext::skip_macro_directive_chars_(ConstStringRegion *input) noexcept
{
	skip_ws(input);
	return input->length() > 1 && *(*input)++ == DIRECTIVE_CHAR;
}

bool
FileContext::directive_(ConstStringRegion input) const
{
	const auto recover = input;
	try {
		if (skip_macro_directive_chars_(&input)) {
			auto h = search_directive_handler_(*input);
			if (h) {
				++input;
				return (this->*h)(input);
			}
		}
		return false;
	}
	catch (SyntaxError &ex) {
		error(ex.what());
	}
	catch (IncludeProcessor::Looped &ex) {
		error("including file \"" + ex.get_name() + "\" is looped:",
		      [this,&ex](std::ostream &os) {
			      os << '\t';
			      this->show_include_record_(
				      os,
				      ex.get_name(),
				      m_input_file_name,
				      m_line_number);
			      os << std::endl;
			      this->dump_include_stack(os);
		      });
	}
	catch (PathList::CannotOpen &ex) {
		error("file \""+ex.get_name() + "\" cannot be opened.");
	}
	output_stream() << std::string(recover) << std::endl;
	return true;
}

std::string
FileContext::expand_(ConstStringRegion input) const
{
	auto dbcs = false;
	auto macro_char = false;
	auto enter_out = false;
	std::string out;

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
				out += ConstStringRegion(begin, --tmp);
				out += *input;
				enter_out = true;
				begin = ++input;
				continue;
			}
			auto recovery = input;
			out += ConstStringRegion(begin, --recovery);
			enter_out = true;
			try {
				auto macroname = get_macro_name(&input);
				if (!input.is_end() && isspace(*input))
					++input;
				auto locker =
				    macro_processor().lock(macroname);
				out += this->expand_(locker.query());
			}
			catch (MacroProcessor::Looped &ex) {
				error("macro \""_s +
				      MACRO_CHAR + ex.get_name() +
				      "\" is looped:",
				      [this](std::ostream &os) {
					      dump_macro_stack(os);
				      });
				out += ConstStringRegion(recovery, input);
			}
			catch (MacroProcessor::Undefined &ex) {
				error("macro \""_s +
				      MACRO_CHAR + ex.get_name() +
				      "\" is not defined.");
				out += ConstStringRegion(recovery, input);
			}
			catch (SyntaxError &ex) {
				error(ex.what());
				out += ConstStringRegion(recovery, input);
			}
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
		out += begin;
		return out;
	}
	return saved;
}

void
FileContext::process_()
{
	try {
		while (m_input_stream.good()) {
			std::string input;

			getline(m_input_stream, input);
			m_line_number++;
			if (!this->directive_(input)) {
				if (m_compile_unit_context.is_auto_scope() &&
				    input.size() > 0 &&
				    isalpha((int)(unsigned char)input[0])) {
					macro_processor().set_scope(
						std::string(1, input[0]));
				}
				output_stream() << this->expand_(input)
						<< std::endl;
			}
			if (output_stream().bad())
				throw FatalError("cannot write to file \"" +
						 get_output_file_name() + "\"");
		}
	}
	catch (FatalError &ex) {
		put_message(s_fatal, ex.what());
		throw ExitFailure();
	}
}

namespace
{

//
// f_banner : whether show the banner.
//
auto f_banner = true;

//
// banner : show banner once.
//
void
banner()
{
	if (f_banner)
		std::cerr << "macro for mck (c)2006-2015 T.SHIOZAKI."
			  << std::endl;
	f_banner = false;
}

//
// usage : show banner and usage.
//
void
usage()
{
	banner();
	std::cerr << "usage: mckmacro [-q] [-o outfile] [infile]"
		  << std::endl;
	exit(EXIT_FAILURE);
}

//
// warnx : show warning message.
//
void
warnx(const char *fmt)
{
	banner();
	std::cerr << fmt << std::endl;
}

//
// errx : show error message and exit.
//
void
errx(int excode, const char *fmt)
{
	warnx(fmt);
	exit(excode);
}

} // namespace <anonymous>

template <class BaseStream> struct StreamTraits;

template <>
struct StreamTraits<std::istream>
{
	static constexpr const char *stdio_name = "<stdin>";
	static constexpr std::istream &stdio_stream = std::cin;
	using FileStream = std::ifstream;
};

template <>
struct StreamTraits<std::ostream>
{
	static constexpr const char *stdio_name = "<stdout>";
	static constexpr std::ostream &stdio_stream = std::cout;
	using FileStream = std::ofstream;
};


//
// FileArg : file argument handler.
//
//  to open file corresponding to the file name, or reference to stdio
//  if the argument is "-".
//
template <class BaseStream>
class FileArg
{
	NONCOPYABLE(FileArg);
private:
	using StreamTraits = struct StreamTraits<BaseStream>;
	using FileStream = typename StreamTraits::FileStream;
public:
	FileArg() = default;
	bool is_set() const noexcept { return m_is_file; }
	void set_file_name(std::string fname) noexcept
	{
		m_is_file = true;
		m_file_name = std::move(fname);
	}
	const std::string &get_file_name() const noexcept
	{
		return m_file_name;
	}
	void open()
	{
		if (m_is_file && m_file_name != "-") {
			m_file_stream.open(m_file_name);
			if (m_file_stream.fail())
				throw FatalError(
					"cannot open file \""_s+
					m_file_name+"\".");
			m_result_stream = &m_file_stream;
		} else {
			m_file_name = StreamTraits::stdio_name;
			m_result_stream = &StreamTraits::stdio_stream;
		}
	}
	BaseStream &get_stream() const
	{
		if (!m_result_stream)
			errx(EXIT_FAILURE,
			     "error: internal error in FileArg:get().");
		return *m_result_stream;
	}
private:
	bool m_is_file = false;
	std::string m_file_name;
	FileStream m_file_stream;
	BaseStream *m_result_stream = nullptr;
};

int
main(int argc, char **argv)
{
	FileArg<std::istream> input;
	FileArg<std::ostream> output;
	auto done = false;
	auto error_as_fatal = false;
	auto warning_as_error = false;
	int ret = EXIT_SUCCESS;

	argv++;
	argc--;
	while (!done && argc > 0 && *argv[0] == '-') {
		auto ilopt = [&argv]() {
			warnx(("error: unknown option "_s +
			       *argv + ".").c_str());
			usage();
		};
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
			if (output.is_set())
				errx(EXIT_FAILURE,
				     "error: duplicate -o option.");
			argv++;
			argc--;
			output.set_file_name(argv[0]);
			break;
		case 'W': {
			const std::string opt = &argv[0][2];
			if (opt == "fatal")
				error_as_fatal = true;
			else if (opt == "error")
				warning_as_error = true;
			else
				ilopt();
		}
			break;
		default:
			ilopt();
		}
		argv++;
		argc--;
	}
	if (argc > 1) {
		warnx("error: too many input files.");
		usage();
	} else if (argc == 1)
		input.set_file_name(argv[0]);

	banner();

	try {
		try {
			input.open();
			output.open();
		}
		catch (FatalError &ex) {
			std::cerr << "fatal: " << ex.what() << std::endl;
			throw ExitFailure();
		}

		CompileUnitContext ctx(output.get_file_name(),
				       output.get_stream(),
				       std::cerr);

		ctx.set_error_as_fatal_mode(error_as_fatal);
		ctx.set_warning_as_error_mode(warning_as_error);

		FileContext::process(ctx,
				     input.get_file_name(),
				     input.get_stream());

		if (ctx.get_error_count() || ctx.get_warn_count()) {
			std::cerr << ctx.get_error_count() << " errors, "
				  << ctx.get_warn_count() << " warnings."
				  << std::endl;
			if (ctx.get_error_count())
				throw ExitFailure();
		}
	}
	catch (ExitFailure &ex) {
		errx(EXIT_FAILURE, "compile failed.");
		ret = EXIT_FAILURE;
	}

	return ret;
}
