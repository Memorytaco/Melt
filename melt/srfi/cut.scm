(library (melt srfi cut)
         (export cut)
         (import (scheme))
         
         (define-syntax cut
           (syntax-rules (<>)
             [(_ procedure )]))
         
         )
