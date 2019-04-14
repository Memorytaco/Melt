(library
  (melt structure)
  (export type-parser
          type-post
          type-renderer
          type-page

          type-site
          type-asset

          type-trigger)

  (import (scheme))

  ;; it now is an uniform utility! can be stroed in
  ;; one place and use multiple times!
  (module type-parser
          [make-parser
            parser?
            parser-type
            parser-proc
            parser-refp]
          (define-record-type
            parser
            (nongenerative melt-parser)
            (fields
              ;; the symbol which means the file type
              ;; the symbol is used as file extension
              (immutable type parser-type)
              ;; proc is the procedure which take charge with
              ;; the source file
              (immutable proc parser-proc)
              ;; refp==>refine procedure : update the resource file
              ;; it need to be designed carefully, because it will alter
              ;; the source file
              (immutable refp parser-refp))))

  ;; there maybe a lot of procedure between
  ;; this two components.

  ;; the post recieve the data from parser
  ;; and then process the data to satisfy its
  ;; need. So the data stored in a post is all
  ;; the data one post needs. No more change but
  ;; use.

  ;; the meta and attr is all an assoc list
  (module type-post
          [make-post
            post?
            post-meta post-meta-set!
            post-attr post-attr-set!
            post-cont post-cont-set!]
          (define-record-type
            post
            (nongenerative melt-post)
            (fields
              ;; it contains the attribute about the
              ;; source file!
              ;; the meta and attr are all the data
              ;; but cont is sxml tree
              (mutable meta post-meta post-meta-set!)
              (mutable attr post-attr post-attr-set!)
              (mutable cont post-cont post-cont-set!))))

  ;; used to render the page component
  (module type-renderer
          [make-renderer
            renderer?
            renderer-type
            renderer-proc proc-set!
            renderer-data data-set!]
          (define-record-type
            renderer
            (nongenerative melt-renderer)
            (fields
              ;; the type is an unique id to distinguish the render
              (immutable type renderer-type)
              ;; proc==>process process function used to render the
              ;; page
              (mutable proc renderer-proc proc-set!)
              ;; data is the data which maybe be needed, it's the data type.
              (mutable data renderer-data data-set!))))

  ;; page is used to compose one page
  ;; and use the proc to write ti to disk
  ;; all the information relevant should
  ;; be done before page, page only store
  ;; information about the page it self.
  (module type-page
          [make-page
            page?
            page-meta page-meta-set!
            page-cont page-cont-set!
            page-comt page-comt-set!]
          (define-record-type
            page
            (nongenerative melt-page)
            (fields
              ;; meta ==> store some useful value for the page
              ;; cont ==> is the template for the page; actually it
              ;; is a procedure accept itself a page obj, and generate
              ;; sxml tree
              ;; comt ==> it is a list of symbols map the renderer type
              ;; need to be registered first
              (mutable meta page-meta page-meta-set!)
              (mutable cont page-cont page-cont-set!)
              (mutable comt page-comt page-comt-set!))))

  ;; site type is only for definition
  (module type-site
          [make-site
            site?
            site-layout layout-set!
            site-comt comt-set!
            site-attr attr-set!]
          (define-record-type
            site
            (nongenerative melt-site)
            (fields
              ;; it stores data type data
              ;; this defines how the published site to be generated!
              (mutable layout site-layout layout-set!)
              ;; comt==>component : it describes the composement of the
              ;; site and the action on each component. for example: the site map
              ;; it's also a data type
              (mutable comt site-comt comt-set!)
              ;; it is the attribute of the site like domain name
              ;; it is a data type
              (mutable attr site-attr attr-set!))))

  (module type-asset
          [make-asset
            asset?
            asset-source
            asset-target]
          (define-record-type
            asset
            (nongenerative melt-asset)
            (fields
              (immutable source asset-source)
              (immutable target asset-target))))

  ;; the trigger module for future
  (module type-trigger
          [make-trigger
            trigger?
            trigger-cond
            trigger-act]
          (define-record-type
            trigger
            (nongenerative melt-trigger)
            (fields
              (immutable cond trigger-cond)
              (immutable act  trigger-act))))


  )
