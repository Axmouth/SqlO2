import { TestBed } from '@angular/core/testing';

import { SqlO2Service } from './sqlo2.service';

describe('sqlo2Service', () => {
  let service: SqlO2Service;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(SqlO2Service);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
