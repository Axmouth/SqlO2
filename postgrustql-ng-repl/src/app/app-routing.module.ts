import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ReplComponent } from './components/repl/repl.component';

const routes: Routes = [{ path: '**', component: ReplComponent, pathMatch: 'full' }];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
