"""repathlib module
"""

import os
import pathlib
import re

from typing import Generator, Match, Optional, Pattern, Tuple, Union


__all__ = ['PosixPath', 'WindowsPath', 'Path']


def _create_search_match(func: str):
    """Create a search or match method for the class

    Parameters
    ----------
    func: str, {'search', 'match', 'fullmatch', 'findall', 'finditer'}
        re function to create
    """

    def _search_match(
        self,
        pattern: Union[str, Pattern],
        part: str='name',
        flags: int=0
    ) -> Optional[Match]:
        """Pass the given pattern, part and flags to ``func``
        """

        # Define the special cases for ``part``
        parts_dict = {
            'full': str(self),
            'suffixes': '.'.join(self.suffixes),
            'resolved': str(self.resolve())
        }

        # Get the string we need to search--either a special case
        # or the ``part`` attribute of the instance
        string = parts_dict.get(part, str(getattr(self, part)))

        if isinstance(pattern, str):
            return getattr(re, func)(pattern, string, flags)
        return getattr(pattern, func)(string, flags)

    doc_string = """Apply :py:func:`re.{}` to the path

    Parameters
    ----------
    pattern : Union[str, Pattern]
        Regular expression string, or a compiled regular expression
    part : str, optional {{'name', 'full', 'suffix', 'suffixes', 'parent', 'stem', 'resolved'}}, default 'name'
        Part of the Path to search; ``full`` is the complete (but
        unresolved) path, ``suffixes`` is all suffixes joined with
        ``.``, ``resolved`` is the complete resolved path, and the
        remaining options are exactly that property or attribute of
        the Path
    flags : int, optional, default 0
        Flags passed to {}
        
    Returns
    -------
    Optional[Match]
        Match of the pattern to the given part of the path, or None
    """.format(func.lower(), func.lower())

    if func == 'match':
        doc_string += """
    Warnings
    --------
    Note that this method is ``match_``, not ``match``, to maintain the
    existing method :py:meth:`pathlib.PurePath.match`.        
    """

    _search_match.__doc__ = doc_string
    _search_match.__name__ = func

    return _search_match


class RePath(pathlib.Path):
    """Subclass of pathlib.Path with functions from re
    """

    search = _create_search_match('search')
    match_ = _create_search_match('match')
    fullmatch = _create_search_match('fullmatch')
    findall = _create_search_match('findall')
    finditer = _create_search_match('finditer')

    def reiterdir(
        self,
        pattern: Union[str, Pattern]=None,
        method: str='search',
        part: str='name',
        yield_type: str='path',
        flags=0
    ) -> Generator[Union['RePath', Match, Tuple['RePath', Match]], None, None]:
        """Iterate over the path, yielding paths which match the given
        pattern. If no pattern is given, defaults to using Path.iterdir.

        Parameters
        ----------
        pattern : str, default None
            Regular expression string, or a compiled regular expression
        method : str, optional {'search', 'match', 'fullmatch', 'findall', 'finditer'}, default 'search'
            Method from ``re`` to use
        part : str, optional {'name', 'full', 'suffix', 'suffixes', 'parent', 'stem', 'resolved'}, default 'name'
            Part of the Path to search; ``full`` is the complete (but
            unresolved) path, ``suffixes`` is all suffixes joined with
            ``.``, ``resolved`` is the complete resolved path, and the
            remaining options are exactly that property or attribute of
            the Path
        yield_type : str, optional ['path', 'match', 'tuple'], default 'path'
            Yield type of the generator. If ``path``, returns the Path
            object; if ``match``, returns the re.Match object; if
            ``tuple``, returns a tuple of (Path, re.Match)
        flags : int, optional, default 0
            Flags passed to the re function

        Yields
        ------
        Generator[Union[Path, Match, Tuple[Path, Match]], None, None]
            If ``yield_type`` is 'path' (the default), yields
            Path instances; if 'match', yields matches; if
            'tuple', yields a tuple of path and match
        """

        # Rename `match` to `match_`
        method = 'match_' if method == 'match' else method

        if pattern is None:
            yield from super().iterdir()
        else:
            for path in super().iterdir():
                match = getattr(path, method)(pattern, part, flags)
                if match:
                    yield {
                        'path': path,
                        'match': match,
                        'tuple': (path, match)
                    }[yield_type]


class PosixPath(pathlib.PosixPath, RePath):
    pass


class WindowsPath(pathlib.WindowsPath, RePath):
    pass


class Path(pathlib.Path):
    """PurePath subclass that can make system calls.
    Path represents a filesystem path but unlike PurePath, also offers
    methods to do system calls on path objects. Depending on your system,
    instantiating a Path will return either a PosixPath or a WindowsPath
    object. You can also instantiate a PosixPath or WindowsPath directly,
    but cannot instantiate a WindowsPath on a POSIX system or vice versa.
    """
    __slots__ = (
        '_accessor',
        '_closed',
    )

    def __new__(cls, *args, **kwargs):
        if cls is Path:
            cls = WindowsPath if os.name == 'nt' else PosixPath
        self = cls._from_parts(args, init=False)
        if not self._flavour.is_supported:
            raise NotImplementedError("cannot instantiate %r on your system"
                                      % (cls.__name__,))
        self._init()
        return self
