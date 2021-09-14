import { Injectable, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable, combineLatest } from 'rxjs';
import { first } from 'rxjs/operators';

export function waitFor<T>(signal$: Observable<any>) {
  return (source$: Observable<T>) =>
    new Observable<T>((observer) => {
      // combineLatest emits the first value only when
      // both source and signal emitted at least once
      combineLatest([source$, signal$.pipe(first())]).subscribe(([v]) => observer.next(v));
    });
}

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

@Injectable({
  providedIn: 'root',
})
export class PostgrustqlService implements OnDestroy {
  notifier = new Subject();
  private wasm: typeof import('../../../../wasm_client/pkg/postgrustql_wasm_client.js');
  private loaded = false;
  loading: Subject<boolean> = new Subject<boolean>();

  constructor() {
    import('../../../../wasm_client/pkg/postgrustql_wasm_client.js').then((wasm) => {
      console.log('this.loaded');
      this.wasm = wasm;
      this.loaded = true;
      this.loading.next(true);
      this.loading.complete();
    });
  }
  ngOnDestroy(): void {
    this.notifier.next();
    this.notifier.complete();
  }

  async eval(queryString: string): Promise<any> {
    while (this.loaded !== true) {
      await sleep(300);
    }
    return this.wasm.eval_raw(queryString);
  }
}
